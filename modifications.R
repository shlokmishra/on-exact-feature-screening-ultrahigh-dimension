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
