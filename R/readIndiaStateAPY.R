#' @title readIndiaStateAPY
#' This function reads foodcrops data for India downloaded from:
#'  UPAg - Unified Portal for Agricultural Statistics by  Department of Agriculture & Farmers Welfare
#' @author Ankit Saha
#' @importFrom readr read_csv
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter %>% select mutate bind_rows everything
#' @importFrom magclass as.magpie
#' @examples
#' \dontrun{ a <- madrat::readSource(type="IndiaAPY",subtype="Rice",convert="onlycorrect") }
#' @return magpie object containing Area, Yield, and Production data.


#library(dplyr)
#library(tidyr)
#library(readr)
#library(magclass)


##setwd("W:/PIK R/inputdata/sources/IndiaStateAPY") ## For local runs

# Function to read and merge CSV files from current working directory (set by user),
# then convert the merged dataframe to a magpie object

readIndiaStateAPY <- function() {

  # List all CSV files in the current working directory
  l <- list.files(pattern = "\\.csv$", full.names = FALSE)

  if (length(l) == 0) {
    stop("No CSV files found in the current working directory.")
  }

  # Function to read and reshape individual CSV files
  .reshapeCSV <- function(file) {

    # Declare variables to suppress R CMD check notes
    Year <- NULL

    # Extract state from the filename (portion before first '-')
    state <- sub("-.*", "", basename(file))

    # Read CSV
    x <- read_csv(file, show_col_types = FALSE)

    # Clean column names by removing trailing '-XX'
    colnames(x) <- gsub("-\\d{2}$", "", colnames(x))

    # Convert to long format, drop Season, pivot columns to variables and years
    x <- x %>%
      select(-"Season") %>%                # Remove Season if present
      pivot_longer(
        cols = -"Crop",
        names_to = c(".value", "Year"),
        names_sep = "-"
      ) %>%
      mutate(
        state = state,                  # Add state column
        #Year = as.numeric(sub(".*-", "", x$Year)) + 1  # Parse year and adjust
        Year = as.numeric(Year) + 1
      ) %>%
      select(state, everything())       # Move state to first column

    return(x)
  }

  # Read, process, and merge all CSV files into a single dataframe
  out <- bind_rows(lapply(l, .reshapeCSV))

  # Convert merged dataframe to magpie object with spatial key 'state'
  out <- as.magpie(out, spatial = "state")

  return(out)
}

## m <- readIndiaStateAPY() ## To test output


