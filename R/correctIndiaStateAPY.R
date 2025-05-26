#' @title correctIndiaStateAPY
#' @description Correct IndiaStateAPY data
#' @param x magpie object provided by the read function
#' @return magpie objects with corrected IndiaStateAPY data
#' @author Ankit Saha
#' @examples
#' \dontrun{
#' readSource("IndiaAPY", convert = "onlycorrect")
#' }
#'
#' @importFrom magclass as.magpie as.data.frame
#' @importFrom utils read.csv
#' @importFrom madrat toolCountryFill
#' @importFrom dplyr  %>% select filter mutate rename case_when
#' @importFrom tidyr replace_na


##library(dplyr)
##library(magclass)
##library(tidyr)

# Function to correct India State APY magpie data
correctIndiaStateAPY <- function(x) {
  # Declare variables to suppress R CMD check notes
  Value <- NULL

  # Convert magpie object to data frame
  x <- as.data.frame(x) %>%
    select(-"Cell") %>%
    mutate(Value = replace_na(Value, 0))  # Convert NA values to 0

  # Rename columns to match desired names
  x <- x %>%
    rename(
      state = "Region",
      year = "Year",
      crop = "Data1",
      variable = "Data2",
      value = "Value"
    )
  # Ensure 'value' is numeric
  #x$value <- as.numeric(as.character(x$value))

  # Define unit column based on variable
  x <- x %>%
    mutate(unit = case_when(
      variable == "Area" ~ "1000ha",
      variable == "Production" ~ "1000t",
      variable == "Yield" ~ "kg/ha",
      TRUE ~ "unknown"
    ))

  # Reorder columns to place 'unit' just before 'value'
  x <- x %>%
    select("state", "year", "crop", "variable", "unit", "value")

  # Make 'unit' a factor for consistent levels
  x$unit <- factor(x$unit, levels = c("1000ha", "1000t", "kg/ha", "unknown"))

  # Define the items to exclude to avoid aggregates and less important crops
  items_to_exclude <- c("Shree Anna /Nutri Cereals",
                        "Nutri/Coarse Cereals",
                        "Cereals",
                        "Total Pulses",
                        "Total Food Grains",
                        "Total Oil Seeds",
                        "Jute & Mesta",
                        "Jute", "Mesta", "Tobacco", "Sannhemp", "Guarseed"
                        )

  # Filter out specified crops from the dataframe
  x <- x %>%
    filter(!x$crop %in% items_to_exclude)

  # Convert production from bales to 1000t for specific crops
  # Apply only where variable == "Production"
  x <- x %>%
    mutate(value = case_when(
      variable == "Production" & crop == "Cotton" ~ value * 0.17,
      variable == "Production" & crop %in% c("Jute", "Sannhemp", "Mesta") ~ value * 0.18,
      TRUE ~ value
    ))

  # Add a new column for units based on the variable
  #x <- x %>%
    #mutate(unit = case_when(
     #variable == "Area" ~ "1000ha",
     #variable == "Production" ~ "1000t",
     #variable == "Yield" ~ "kg/ha",
     #TRUE ~ NA_character_  # Assign NA for any other cases
    #))

  # Important: For as.magpie to interpret the cell data from 'value' column,
  # rename 'value' to '.value' before conversion.
  # This is the convention that magclass uses, to distinguish data column from dimension columns.

  x <- x %>%
    rename(.value = "value")

  # Convert back to magpie object, specifying spatial key "state"
  x <- as.magpie(x, spatial = "state")

  # Mapping the states within India
   mapping <- read.csv(system.file("extdata", "regional/mappingIndiaStateAPY.csv", package = "mrfable"))
   x <- toolCountryFill(x, countrylist = as.vector(mapping[, "state"]))


  return(x)
}
##n <- correctIndiaStateAPY(m)
##str(n)
