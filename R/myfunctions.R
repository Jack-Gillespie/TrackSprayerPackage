#' Calculate Standard Curve
#'
#' This function calculates the standard curve.
#'
#' @param df Dataframe
#' @param rep_value Replication value
#' @param bottle_number Bottle number
#'
#' @return NULL
#'
#' @export
#'
#'
#'
CalculateStandardCurve <- function(df, rep_value, bottle_number) {
rep_standards <- df %>%
  filter(rep == rep_value) %>%
  filter(extraction_solution_bottle_number == bottle_number) %>%
  filter(grepl("standard", surface))

RepRegression <- lm(rfu ~ volume_pipetted, data = rep_standards)
RepSlope <- RepRegression$coefficients["volume_pipetted"]
RepRsquared <- summary(RepRegression)$r.squared

cat("Analysis for extraction bottle", bottle_number, "\n")
cat("Here is gradient: ", RepSlope, "\n")
cat("Here is the rsquared: ", RepRsquared, "\n")

RepStandard_Plot <- ggplot(rep_standards, aes(y = rfu, x = volume_pipetted)) +
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE) +
  geom_text(x = max(rep_standards$volume_pipetted), y = max(rep_standards$rfu), label = paste0("Slope: ", round(RepSlope, 3)), vjust = 3, hjust = 5) +
  geom_text(x = max(rep_standards$volume_pipetted), y = max(rep_standards$rfu), label = paste0("R-squared: ", round(RepRsquared, 3)), vjust = 1, hjust = 4.5) +
  ylab("RFU") +
  xlab("µl Pipetted")

print(RepStandard_Plot)
result_data <- data.frame(rep_value = rep_value, bottle_number = bottle_number, RepSlope = RepSlope, RepRsquared = RepRsquared)

# Use <<- to update the global variable
slope_table <<- rbind(slope_table, result_data)

return(NULL)
}

#' Calculate volume deposited
#'
#' This function calculates the volume deposited by the track sprayer
#'
#' @param df Dataframe
#' @param rep_value Replication value
#' @param bottle_number Bottle number
#' @param slope_table slope table
#'
#' @return Dataframe with application rate information
#'
#' @export
CalculateVolumeDeposited <- function(df, rep_value, bottle_number, slope_table) {

  RepSlope <- slope_table$RepSlope[slope_table$rep_value == rep_value & slope_table$bottle_number == bottle_number]

  if (length(RepSlope) == 0) {
    cat("Warning: RepSlope not found for rep", rep_value, "and bottle", bottle_number, "\n")
    return(NULL)
  }

  rep_negative <- df %>%
    filter(rep == rep_value, extraction_solution_bottle_number == bottle_number, grepl("standards", surface)) %>%
    filter(volume_pipetted == 0) %>%
    select(rfu)

  if (nrow(rep_negative) == 0) {
    cat("Warning: No negative control found for rep", rep_value, "and bottle", bottle_number, "\n")
    return(NULL)
  }

  cat("rep_negative:", rep_negative$rfu, "\n")

  rep_data <- df %>%
    filter(rep == rep_value, extraction_solution_bottle_number == bottle_number, grepl("filter paper", surface)) %>%
    mutate(VolumeDepositedOnSurfaceµl = (rfu - rep_negative$rfu) / RepSlope)

  return(rep_data)
}

#' Calculate Application Rate for filter paper
#'
#' This function calculates the application rate for filter paper.
#'
#' @param df Dataframe
#' @param rep_value Replication value
#' @param bottle_number Bottle number
#'
#' @return Dataframe with application rate information
#'
#' @export
#'
CalculateApplicationRateFilterPaper <- function(df, rep_value, bottle_number) {
  print(paste("Filtered Data for rep:", rep_value, "and bottle:", bottle_number))
  rep_data <- df %>%
    filter(extraction_solution_bottle_number %in% c((bottle_number - 1) * 2 + 1, (bottle_number - 1) * 2 + 2), surface == "filter paper") %>%
    mutate(ApplicationRateMlm2 = (VolumeDepositedOnSurfaceµl / 63.62) * (10000 / 1000))

  if (nrow(rep_data) == 0) {
    cat("Warning: No data for rep", rep_value, "and bottle", bottle_number, "\n")
    return(NULL)
  }

  return(rep_data)
}
