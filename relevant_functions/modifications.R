

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


