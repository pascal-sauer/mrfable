library(dplyr)
library(magclass)
library(tidyr)

calcIndiaStateFoodcrop <- function(subtype = "Area") {

  # Validate input subtype
  if (!subtype %in% c("Area", "Yield", "Production")) {
    stop("choose a valid subtype")
  }

  # Read the data source (assumed to be already correctly processed)
  x <- readSource("IndiaStateAPY", convert = "onlycorrect")

  weight <- NULL

  if (subtype == "Yield") {
    weight <- x[, , "Area"]
    # Avoid division by zero: set Production to 0 where Area is zero
    x[, , "Production"][x[, , "Area"] == 0] <- 0
    # Replace zeros in Area to a very small number to enable division
    x[, , "Area"][x[, , "Area"] == 0] <- 10^-10
    # Calculate yield as production per area scaled by 1000
    x <- collapseNames(1000 * x[, , "Production"] / x[, , "Area"], preservedim = c(2,3))
    # Replace NA in weight with zero
    weight[is.na(weight)] <- 0
  }

  # Replace any remaining NAs in x with zero
  x[is.na(x)] <- 0

  # Return a list with the requested subtype data and auxiliary info
  return(list(
    x = x[, , subtype],
    weight = weight,
    mixed_aggregation = TRUE,
    min = 0,
    max = 999999,
    isocountries = FALSE,
    description = paste0("IndiaStateAPY Foodcrop ", subtype,
                         "Data downloaded from UPAg - Unified Portal for Agricultural Statistics by  Department of Agriculture & Farmers Welfare")
  ))
}
