# Load necessary libraries
library(readr)

# Define function to compute and print the misclassification error
compute_misclassification_error <- function(file_path, example_number) {
  # Read the data without displaying messages
  data <- suppressMessages(read_csv(file_path))
  
  # Compute the average of the "EmarClass" column
  avg_error <- mean(data$EmarClass, na.rm = TRUE) * 100
  
  # Print the misclassification error
  cat(sprintf("Misclassification error for the example %d is %.2f%%\n", example_number, avg_error))
}

# List of files and their corresponding example numbers
files <- list("signals_1.csv", "signals_4.csv", "signals_5.csv", "signals_6.csv", "signals_7.csv")
example_numbers <- c(1, 4, 5, 6, 7)

# Compute and print misclassification error for each file
for(i in 1:length(files)) {
  compute_misclassification_error(files[i], example_numbers[i])
}
