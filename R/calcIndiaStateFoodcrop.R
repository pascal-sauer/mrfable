#' calcIndiaStateFoodcrop
#'
#' Calculates foodcrops data for India downloaded from:
#'  UPAg - Unified Portal for Agricultural Statistics by  Department of Agriculture & Farmers Welfare
#'  https://upag.gov.in/dash-reports/stateprofile?rtab=State+Profile%3A+Crop-wise+APY&rtype=reports
#' @author Ankit Saha
#' @param subtype Area, Yield, or Production
#' @importFrom madrat readSource
#' @importFrom magclass collapseNames
#' @examples
#' \dontrun{
#' a <- madrat::calcOutput("IndiaStateFoodcrop", subtype = "Area")
#' }
#' @return magpie object containing Area, Yield, and Production data.


calcIndiaStateFoodcrop <- function(subtype = "Area") {

  # Validate input subtype
  if (!"Area" %in% c("Area", "Yield", "Production")) {
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

  # Define the mapping of crops in the APY database to magpie crops
  mappingCropsAPY <- as.matrix(data.frame(
    APYcrop = c(
      "Rice", "Wheat", "Maize", "Barley", "Jowar", "Bajra",
      "Ragi", "Small Millets", "Tur", "Gram", "Urad", "Moong",
      "Lentil", "Other Pulses", "Groundnut", "Castorseed",
      "Sesamum", "Nigerseed", "Soybean", "Sunflower",
      "Rapeseed & Mustard", "Linseed", "Safflower", "Sugarcane", "Cotton"
    ),
    k = c(
      "rice_pro", "tece", "maiz", "tece", "trce", "trce",
      "trce", "trce", "puls_pro", "puls_pro", "puls_pro", "puls_pro",
      "puls_pro", "puls_pro", "groundnut", "rapeseed",
      "rapeseed", "rapeseed", "soybean", "sunflower",
      "rapeseed", "rapeseed", "rapeseed", "sugr_cane", "fibres"
    )
  ))

  # Aggregate the data to magpie crops
  x <- toolAggregate(x, rel = mappingCropsAPY, from = "APYcrop", to = "k", dim = 3.1)

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

