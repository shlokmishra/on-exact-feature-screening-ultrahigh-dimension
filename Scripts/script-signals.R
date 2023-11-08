# Vector of file names
csv_files <- c('signals_1.csv', 'signals_2.csv', 'signals_4.csv', 'signals_5.csv',
               'signals_6.csv', 'signals_7.csv', 'signals_8.csv')

# Function to calculate and print the mean for 1st and 7th columns
calculate_and_print <- function(file_name) {
  # Read CSV file
  data <- read.csv(file_name, header=TRUE)
  
  # Check if the file has at least 7 columns
  if (ncol(data) < 7) {
    cat(sprintf("File %s has less than 7 columns.\n", file_name))
    return()
  }
  
  # Calculate mean for 1st and 7th columns and multiply by 100
  mean_col1 <- mean(data[[1]], na.rm = TRUE) * 100
  mean_col7 <- mean(data[[7]], na.rm = TRUE) * 100
  
  # Print the results
  cat(sprintf("For file %s:\n", file_name))
  cat(sprintf("Mean of 1st column multiplied by 100: %f\n", mean_col1))
  cat(sprintf("Mean of 7th column multiplied by 100: %f\n\n", mean_col7))
}

# Apply the function to each file in the csv_files vector
sapply(csv_files, calculate_and_print)

csv_files[[1,]]
