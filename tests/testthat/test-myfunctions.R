library(testthat)
library(TrackSprayerPackage)
library(readxl)
library(tidyverse)

df1<-read_xlsx("2024-01-03 Track Sprayer Calibration and Preliminary Data Record Sheet.xlsx")
df2<-read_xlsx("2024-01-05 Track Sprayer Calibration and Preliminary Data Record Sheet.xlsx")
df3<-read_xlsx("2024-01-09 Track Sprayer Calibration and Preliminary Data Record Sheet.xlsx")
df4<-read_xlsx("2024-01-22 Track Sprayer Calibration and Preliminary Data Record Sheet.xlsx")

df1 <- rbind(df1, df2, df3, df4)

glimpse(df1)

df1$rep <- factor(df1$rep, levels = unique(df1$rep))
df1$date <- factor(df1$date, levels = unique(df1$date))
df1$extraction_solution_bottle_number <- factor(df1$extraction_solution_bottle_number, levels = unique(df1$extraction_solution_bottle_number))
df1$surface <- factor(df1$surface, levels = unique(df1$surface))
df1$volume_pipetted <- as.numeric(df1$volume_pipetted)
df1$rfu <- as.numeric(df1$rfu)
df1$column <- factor(df1$column, levels = unique(df1$column))
df1$speed <- factor(df1$speed)

slope_table <- data.frame(rep_value = numeric(), bottle_number = numeric(), RepSlope = numeric(), RepRsquared = numeric())

reps <- 1:4

for (rep_value in unique(reps)) {
  corresponding_bottles <- (rep_value - 1) * 2 + 1:2

  for (bottle_number in corresponding_bottles) {
    result_data <- CalculateStandardCurve(df1, rep_value, bottle_number)
    slope_table <- rbind(slope_table, result_data)
  }
}

print(slope_table)

results <- list()

for (rep_value in unique(df1$rep)) {
  corresponding_bottles <- ((as.numeric(rep_value) - 1) * 2 + 1):((as.numeric(rep_value) - 1) * 2 + 2)

  for (bottle_number in corresponding_bottles) {
    cat("Checking rep", rep_value, "and bottle", bottle_number, "\n")

    # Call the function with slope_table
    result <- CalculateVolumeDeposited(df1, rep_value, bottle_number, slope_table)

    # Save the result
    results[[paste(rep_value, bottle_number, sep="_")]] <- result
  }
}

result_list <- list()

print(result_list)

for (rep_value in unique(as.numeric(final_result$rep))) {
  corresponding_bottles <- (rep_value - 1) * 2 + 1:2

  for (bottle_number in corresponding_bottles) {
    rep_data <- CalculateApplicationRateFilterPaper(final_result, rep_value, bottle_number)

    if (!is.null(rep_data)) {
      cat("Results for rep:", rep_value, "and bottle:", bottle_number, "\n")
      print(rep_data)
      result_list[[length(result_list) + 1]] <- rep_data
    } else {
      cat("No data for rep:", rep_value, "and bottle:", bottle_number, "\n")
    }
  }
}

# If you want to combine all results into a single data frame
final_results_df <- do.call(rbind, result_list)
