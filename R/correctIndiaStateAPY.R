library(dplyr)
library(magclass)
library(tidyr)

# Function to correct India State APY magpie data
correctIndiaStateAPY <- function(x) {
  # Convert magpie object to data frame
  x <- as.data.frame(x) %>%
    select(-"Cell") %>%
    mutate(Value = replace_na(Value, 0))  # Convert NA values to 0

  # Rename columns to match desired names
  x <- x %>%
    rename(
      state = Region,
      year = Year,
      crop = Data1,
      variable = Data2,
      value = Value
    )

  # Define the items to exclude to avoid aggregates
  items_to_exclude <- c("Shree Anna /Nutri Cereals",
                        "Nutri/Coarse Cereals",
                        "Cereals",
                        "Total Pulses",
                        "Total Food Grains",
                        "Total Oil Seeds",
                        "Jute & Mesta")

  # Filter out specified crops from the dataframe
  x <- x %>%
    filter(!crop %in% items_to_exclude)

  # Convert production from bales to 1000t for specific crops
  # Apply only where variable == "Production"
  x <- x %>%
    mutate(value = case_when(
      variable == "Production" & crop == "Cotton" ~ value * 0.17,
      variable == "Production" & crop %in% c("Jute", "Sannhemp", "Mesta") ~ value * 0.18,
      TRUE ~ value
    ))

  # Convert back to magpie object, specifying spatial key "state"
  x <- as.magpie(x, spatial = "state")

  # Mapping the states within India
  ## mapping <- read.csv(system.file("extdata", "regional/mappingIndiaStateAPY.csv", package = "mrfable"))
  ## x <- toolCountryFill(x, countrylist = as.vector(mapping[, "state"]))


  return(x)
}
n <- correctIndiaStateAPY(m)
str(n)
