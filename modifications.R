# Function to format the lists
format_list <- function(lst) {
  sapply(lst, function(x) {
    if(length(x) > 1) {
      return(paste0("{", paste(x, collapse = ","), "}"))
    } else {
      return(as.character(x))
    }
  })
}

ks_test_1d <- function(train.set, n1train, n2train) {
    d = ncol(train.set)
    
    # Initialize an array to hold the KS statistics for each dimension
    ks_stats = rep(0, d)
    
    for (i in 1:d) {
      # Separate the data for the first and second distributions
      data1 = train.set[1:n1train, i]
      data2 = train.set[(n1train + 1):(n1train + n2train), i]
      
      
      # Perform the Kolmogorov-Smirnov test using the built-in ks.test function
      ks_result = ks.test(data1, data2)
      ks_statistic_formatted = sprintf("%.10f", ks_result$statistic)
      
      # Store the KS statistic
      ks_stats[i] = ks_result$statistic
    }
    
    return(ks_stats)
}

ks_test_2d <- function(train.set, n1train, n2train) {
  d = ncol(train.set)
  
  # Initialize a matrix to hold the KS statistics for each pair of dimensions
  ks_matrix = matrix(0, d, d)
  
  for (i in 1:d) {
    for (j in i:d) {
      # Separate the data for the first and second distributions for both dimensions
      data1_dim1 = train.set[1:n1train, i]
      data1_dim2 = train.set[1:n1train, j]
      data2_dim1 = train.set[(n1train + 1):(n1train + n2train), i]
      data2_dim2 = train.set[(n1train + 1):(n1train + n2train), j]
      
      # Assume peacock2 computes the 2D KS test for two sets of 2D data
      ks_result = peacock2(cbind(data1_dim1, data1_dim2), cbind(data2_dim1, data2_dim2))
      
      # Store the KS statistic
      ks_matrix[i, j] = ks_result
      ks_matrix[j, i] = ks_result
      
    }
  }
  
  return(ks_matrix)
}


kolmogorov_smirnov_test_1d <- function(train.set, n1train, n2train) {
  d = ncol(train.set)
  
  # Initialize an array to hold the KS statistics for each dimension
  ks_stats = rep(0, d)
  
  for (i in 1:d) {
    # Separate the data for the first and second distributions
    data1 = train.set[1:n1train, i]
    data2 = train.set[(n1train + 1):(n1train + n2train), i]
    
    # Sort the data for EDF calculation
    data1 = sort(data1)
    data2 = sort(data2)
    
    # Compute the EDFs for both data sets
    edf1 = (1:n1train) / n1train
    edf2 = (1:n2train) / n2train
    
    # Interleave data1 and data2 and compute their corresponding EDF values
    combined_data = c(data1, data2)
    combined_edf1 = approx(data1, edf1, xout=combined_data)$y
    combined_edf2 = approx(data2, edf2, xout=combined_data)$y
    
    # Compute the KS statistic as the maximum absolute difference between the two EDFs
    ks_stats[i] = max(abs(combined_edf1 - combined_edf2), na.rm=TRUE)
  }
  
  return(ks_stats)
}

library(Peacock.test)

multivariate_ks_test <- function(train.set, n1train, n2train, dimension) {
  
  # Separate the data for the first and second distributions
  data1 = train.set[1:n1train, 1:dimension]
  data2 = train.set[(n1train + 1):(n1train + n2train), 1:dimension]
  
  # Initialize the KS statistic
  ks_stat = NULL
  
  if (dimension == 2) {
    # Perform the 2D Peacock test
    ks_result = peacock2(data1, data2)
    ks_stat = ks_result$statistic
  } else if (dimension == 3) {
    # Perform the 3D Peacock test
    ks_result = peacock3(data1, data2)
    ks_stat = ks_result$statistic
  } else {
    stop("Invalid dimension. Please specify either 2 or 3.")
  }
  
  return(ks_stat)
}

xtest <- matrix(rnorm(12, 0, 1), ncol=3)
ytest <- matrix(rnorm(18, 0, 1), ncol=3)
ks3dtest <- peacock3(xtest, ytest)
ks3dtest


library(readr)

# List of file paths
file_paths <- c(
  "signals_1.csv",
  "signals_2.csv",
  "signals_4.csv",
  "signals_5.csv",
  "signals_6.csv",
  "signals_7.csv",
  "signals_8.csv"
)

# Function to calculate the number of exact matching rows for a file
calculate_matching_rows <- function(file_path) {
  df <- read_csv(file_path)
  matching_rows <- nrow(df[df[[1]] == df[[2]],])
  return(matching_rows)
}

# Calculate matching counts for each file
matching_counts <- sapply(file_paths, calculate_matching_rows)
names(matching_counts) <- file_paths

print(matching_counts)


library(readr)
library(stringr)

# Function to extract components from a cell
extract_components <- function(cell) {
  # Extracting pairs
  pairs <- str_extract_all(cell, "\\{\\d+,\\d+\\}") %>% unlist()
  
  # Extracting standalone components
  standalone <- str_extract_all(cell, "(?<!\\{)\\d+(?!\\})") %>% unlist()
  
  return(list(pairs = pairs, standalone = standalone))
}

# Function to count matching pairs and standalone components for a row
count_matches <- function(first_col, second_col) {
  first_components <- extract_components(first_col)
  second_components <- extract_components(second_col)
  
  matching_pairs <- sum(first_components$pairs %in% second_components$pairs)
  matching_standalone <- sum(first_components$standalone %in% second_components$standalone)
  
  return(c(matching_pairs, matching_standalone))
}

# List of file paths
file_paths <- c(
  "signals_1.csv",
  "signals_2.csv",
  "signals_4.csv",
  "signals_5.csv",
  "signals_6.csv",
  "signals_7.csv",
  "signals_8.csv"
)

# Process each file

     