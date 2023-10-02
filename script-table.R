# Load required libraries
install.packages("dplyr")
library(dplyr)

# Set working directory (change this to your directory path)
# setwd("path_to_your_directory")

# List all csv files
csv_files <- list.files(pattern = "*.csv")

# Column numbers as per Excel naming
mars_columns <- c(7, 40, 72)  # G, AN, BU named as g1, g2, g3
mixs_columns <- c(8, 41, 73)  # H, AO, BV named as g1, g2, g3

# Initialize an empty list to store results
results <- list()

# Process each file
for (file in csv_files) {
  # Read CSV file
  df <- read.csv(file)
  
  # Check if the columns exist by their number
  if (all(c(mars_columns, mixs_columns) <= ncol(df))) {
    
    # Multiply values by 100
    df[mars_columns] <- df[mars_columns] * 100
    df[mixs_columns] <- df[mixs_columns] * 100
    
    # Compute mean and sd for MarS columns
    mars_stats <- summarise(df,
                            MarS_g1_mean = mean(df[, mars_columns[1]], na.rm = TRUE),
                            MarS_g1_sd = sd(df[, mars_columns[1]], na.rm = TRUE),
                            MarS_g2_mean = mean(df[, mars_columns[2]], na.rm = TRUE),
                            MarS_g2_sd = sd(df[, mars_columns[2]], na.rm = TRUE),
                            MarS_g3_mean = mean(df[, mars_columns[3]], na.rm = TRUE),
                            MarS_g3_sd = sd(df[, mars_columns[3]], na.rm = TRUE))
    
    # Compute mean and sd for MixS columns
    mixs_stats <- summarise(df,
                            MixS_g1_mean = mean(df[, mixs_columns[1]], na.rm = TRUE),
                            MixS_g1_sd = sd(df[, mixs_columns[1]], na.rm = TRUE),
                            MixS_g2_mean = mean(df[, mixs_columns[2]], na.rm = TRUE),
                            MixS_g2_sd = sd(df[, mixs_columns[2]], na.rm = TRUE),
                            MixS_g3_mean = mean(df[, mixs_columns[3]], na.rm = TRUE),
                            MixS_g3_sd = sd(df[, mixs_columns[3]], na.rm = TRUE))
    
    # Store results
    results[[file]] <- list(mars_stats, mixs_stats)
  }
}

# Print results
for (file in names(results)) {
  cat("File:", file, "\n")
  cat("MarS g1: Mean =", results[[file]][[1]]$MarS_g1_mean, ", SD =", results[[file]][[1]]$MarS_g1_sd, "\n")
  cat("MarS g2: Mean =", results[[file]][[1]]$MarS_g2_mean, ", SD =", results[[file]][[1]]$MarS_g2_sd, "\n")
  cat("MarS g3: Mean =", results[[file]][[1]]$MarS_g3_mean, ", SD =", results[[file]][[1]]$MarS_g3_sd, "\n")
  cat("MixS g1: Mean =", results[[file]][[2]]$MixS_g1_mean, ", SD =", results[[file]][[2]]$MixS_g1_sd, "\n")
  cat("MixS g2: Mean =", results[[file]][[2]]$MixS_g2_mean, ", SD =", results[[file]][[2]]$MixS_g2_sd, "\n")
  cat("MixS g3: Mean =", results[[file]][[2]]$MixS_g3_mean, ", SD =", results[[file]][[2]]$MixS_g3_sd, "\n")
  cat("\n")
}
# 