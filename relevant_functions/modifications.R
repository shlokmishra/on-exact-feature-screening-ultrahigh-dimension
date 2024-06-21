

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
  log_F_hat <- compute_density(x, f_hats)
  log_G_hat <- compute_density(x, g_hats)
  
  DM <- log_F_hat - log_G_hat
  ifelse(DM >= 0, 1, 2)
}

# Assuming a function to compute kernel density using RKDE weights
compute_density_rkde <- function(x, rkde_output, train_data) {
  w <- rkde_output$w  # Weights from RKDE
  # Assuming gauss_kern is modified to compute kernel between x and train_data
  # This requires iterating over all training points or using vectorized operations
  K_x <- vector("numeric", length = length(w))
  for (i in 1:length(w)) {
    # Compute kernel value between x and train_data[i,]
    K_x[i] <- gauss_kern_single_point(x, train_data[i,], rkde_output$h, ncol(train_data))
  }
  # Compute weighted density estimate
  density_estimate <- sum(w * K_x)
  return(density_estimate)
}

# Modified classify_test_obs to use compute_density_rkde
classify_test_obs_rkde <- function(x,f_hats_rkde,g_hats_rkde) {
  log_F_hat <- log(compute_density_rkde(x, f_hats_rkde, train_class1))
  log_G_hat <- log(compute_density_rkde(x, g_hats_rkde, train_class2))
  
  DM <- log_F_hat - log_G_hat
  return(ifelse(DM >= 0, 1, 2))
}

gauss_kern_single_point <- function(x, x_prime, h, d) {
  # Compute the squared Euclidean distance between x and x_prime
  squared_distance <- sum((x - x_prime)^2)
  
  # Compute the Gaussian kernel value using the squared distance
  kernel_value <- (1 / ((2 * pi * h^2)^(d / 2))) * exp(-squared_distance / (2 * h^2))
  
  return(kernel_value)
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






# classify_test_obs_robust <- function(x, f_hats_robust, g_hats_robust) {
#   log_F_hat <- compute_density_robust(x, f_hats_robust) 
#   log_G_hat <- compute_density_robust(x, g_hats_robust)
#   
#   DM <- log_F_hat - log_G_hat
#   classification <- ifelse(DM >= 0, 1, 2)
#   
#   return(c(log_F_hat, log_G_hat, DM, classification))
# }
# results <- t(apply(test_data_subset, 1, function(row) {
#   classify_test_obs_robust(row, f_hats, g_hats)
# }))

multiClassKS <- function(train.set, class.labels) {
  num.classes <- length(unique(class.labels))
  d <- ncol(train.set)
  # Initialize a matrix to store max KS statistics for each feature across class pairs
  maxKSStats <- rep(0, d)
  
  for (l in 1:(num.classes - 1)) {
    for (m in (l + 1):num.classes) {
      # print(paste("l is", l))
      # print(paste("m is", m))
      # Indices for classes l and m
      idxL <- which(class.labels == l)
      idxM <- which(class.labels == m)
      
      # Combine the subsets for the two classes
      combinedSet <- rbind(train.set[idxL, ], train.set[idxM, ])
      
      # Compute KS statistics for the combined set
      ksStats <- ks_test_1d(combinedSet, length(idxL), length(idxM))
      
      # Update max KS statistics if current stats are higher
      maxKSStats <- pmax(maxKSStats, ksStats)
    }
  }
  margE <- maxKSStats
  # Apply the logic for identifying significant features
  sort.margE = sort(margE)
  b.rat = abs(sort.margE[-1] / sort.margE[-length(sort.margE)])
  Ecut = (floor(length(b.rat) / 2)) + which.max(b.rat[-(1:(floor(length(b.rat) /2)))])
  sig.pos.est = which(margE %in% sort.margE[1:(length(sort.margE)) > Ecut]) # selected components
  
  return(sig.pos.est)
}

ks_test_2d_multi <- function(train.set, class.labels) {
  num.classes <- length(unique(class.labels))
  d <- ncol(train.set)
  # Initialize an array to store max KS statistics for each pair of dimensions across class pairs
  maxKSMatrix <- array(0, dim = c(d, d))
  
  for (l in 1:(num.classes - 1)) {
    for (m in (l + 1):num.classes) {
      idxL <- which(class.labels == l)
      idxM <- which(class.labels == m)
      
      combinedSet <- rbind(train.set[idxL, ], train.set[idxM, ])
      n1train <- length(idxL)
      n2train <- length(idxM)
      
      # Compute KS statistics for the combined set
      ksMatrix <- ks_test_2d(combinedSet, n1train, n2train)
      
      # Update max KS statistics if current stats are higher
      maxKSMatrix <- pmax(maxKSMatrix, ksMatrix)
    }
  }
  
  # Assuming the rest of the logic for non-bipartite matching and feature pair selection remains similar
  # Convert KS statistics to dissimilarity
  dissimMAT <- max(maxKSMatrix) - maxKSMatrix
  diag(dissimMAT) <- 0
  
  obj3 <- distancematrix(x = as.matrix(dissimMAT))
  obj4 <- nonbimatch(obj3)
  
  pairs.tmp <- cbind(obj4$matches$Group1.Row, obj4$matches$Group2.Row)
  pairs.tmp1 <- unique(apply(pairs.tmp, 1, function(vec) paste(sort(vec), collapse = '-')))
  Dpairs <- do.call('rbind.data.frame', strsplit(pairs.tmp1, split = '-', fixed = TRUE))
  colnames(Dpairs) <- c('p1', 'p2')
  Dpairs <- apply(Dpairs, 2, as.numeric)
  
  # Select significant pairs
  blockE <- apply(Dpairs, 1, function(vec) maxKSMatrix[vec[1], vec[2]])
  
  sort.blockE <- sort(blockE)
  b.rat <- abs(sort.blockE[-1] / sort.blockE[-length(sort.blockE)])
  Ecut <- (floor(length(b.rat) / 2)) + which.max(b.rat[-(1:(floor(length(b.rat) / 2)))])
  sig.pos.est <- which(blockE %in% sort.blockE[1:(length(sort.blockE)) > Ecut])
  selected_pairs <- Dpairs[sig.pos.est, , drop = FALSE]
  
  # Convert the selected pairs to string format if needed
  # pair_strings <- apply(selected_pairs, 1, function(x) paste(x, collapse = "-"))
  
  return(list(selected_pairs = selected_pairs, sig_positions_est = sig.pos.est, Dpairs = Dpairs))
}



