

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


###CLASSIFIER
compute_density <- function(x, kde_list) {
  epsilon <- .Machine$double.eps^0.5  # A small positive number close to zero
  
  density_logs <- sapply(1:length(kde_list), function(i) {
    pred_val <- predict(kde_list[[i]], x = x[i])
    
    # Use the estimate or the atomic value
    density_estimate <- if(is.atomic(pred_val)) {
      pred_val[1]
    } else {
      pred_val$estimate
    }
    
    # Ensure the density estimate is strictly positive before taking log
    density_estimate <- max(density_estimate, epsilon)
    
    log_val <- log(density_estimate)
    
    return(log_val)
  })
  return(sum(density_logs))
}

# Step 4: Classify Test Data
classify_test_obs <- function(x) {
  log_F_hat <- compute_density(x, f_hats) + log(n / (n + m))
  log_G_hat <- compute_density(x, g_hats) + log(m / (n + m))
  
  DM <- log_F_hat - log_G_hat
  ifelse(DM >= 0, 1, 2)
}


compute_bivariate_density <- function(x, kde_list) {
  log_densities <- sapply(1:length(kde_list), function(i) {
    input_matrix <- matrix(x[((i-1)*2+1):(i*2)], nrow = 1, byrow = TRUE)
    pred_val <- predict(kde_list[[i]], x = input_matrix)
    
    # Ensure density estimates are non-zero by adding epsilon
    density_estimate <- ifelse(is.atomic(pred_val), pred_val, pred_val$estimate) + epsilon
    
    log(density_estimate)
  })
  return(sum(log_densities))
}

classify_bivariate_test_obs <- function(x) {
  log_F_hat <- compute_bivariate_density(x, f_hats) + log(n / (n + m))
  log_G_hat <- compute_bivariate_density(x, g_hats) + log(m / (n + m))
  
  DM <- log_F_hat - log_G_hat
  ifelse(DM >= 0, 1, 2)
}
