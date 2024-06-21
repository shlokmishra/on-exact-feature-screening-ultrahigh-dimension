# Required library for Gaussian kernel
library(stats)

bandwidth_select <- function(x, b_type = 1, sigma = NULL) {
  if (is.null(sigma)) {
    sigma <- log(seq(0.01, 10, length.out = 100))
  }
  
  n <- nrow(x)
  d <- ncol(x)
  
  X <- as.matrix(dist(x)^2)
  
  if (b_type == 1) {  # Least squares cross validation
    Jmin <- Inf
    for (h in sigma) {
      K1 <- (4 * pi * h^2)^(-d/2) * exp(-X / (4 * h^2))
      K2 <- (2 * pi * h^2)^(-d/2) * (exp(-X / (2 * h^2)) - diag(n))
      J <- sum(K1) / (n^2) - 2 / (n * (n - 1)) * sum(K2)
      if (J < Jmin) {
        h_opt <- h
        Jmin <- J
      }
    }
  } else if (b_type == 2) {  # Log-likelihood cross validation
    Jmax <- -Inf
    for (h in sigma) {
      K <- (2 * pi * h^2)^(-d/2) * (exp(-X / (2 * h^2)) - diag(n))
      J <- mean(log(colSums(K) / (n - 1)))
      if (J > Jmax) {
        h_opt <- h
        Jmax <- J
      }
    }
  } else if (b_type == 3) {  # Jakkola heuristics
    X[X == 0] <- Inf
    Y <- apply(X, 2, min)
    h_opt <- sqrt(median(Y))
  }
  
  return(h_opt)
}

# Function for Gaussian kernel
gauss_kern <- function(X, h, d) {
  return(exp(-X / (2 * h^2)) / (sqrt(2 * pi) * h)^d)
}

# Function for parameter selection
parameter_select <- function(K, type) {
  n <- nrow(K)
  
  w <- rep(1/n, n)
  tol <- 1e-8
  
  norm2mu <- sum(w * (K %*% w))
  normdiff <- sqrt(diag(K) + norm2mu - 2 * K %*% w)
  
  J <- mean(normdiff)
  
  while(TRUE) {
    J_old <- J
    w <- 1 / normdiff
    w <- w / sum(w)
    
    # Check for NA or NaN in w
    if (any(is.na(w) | is.nan(w))) {
      warning("NA or NaN values found in weights (w)")
      break
    }
    
    norm2mu <- sum(w * (K %*% w))
    
    # Check for NA or NaN in norm2mu
    if (is.na(norm2mu) | is.nan(norm2mu)) {
      warning("NA or NaN values found in norm2mu")
      break
    }
    
    sqrt_expression <- diag(K) + norm2mu - 2 * K %*% w
    
    # Check for NA or NaN in sqrt_expression
    if (any(is.na(sqrt_expression) | is.nan(sqrt_expression))) {
      warning("NA or NaN values found in sqrt_expression")
      break
    }
    
    # Ensure the expression inside sqrt is non-negative
    if (any(sqrt_expression < 0)) {
      warning("Negative values encountered in square root calculation")
      break
    }
    
    normdiff <- sqrt(sqrt_expression)
    J <- mean(normdiff)
    
    # Check for valid J and J_old before comparing
    if (!is.na(J) && !is.nan(J) && !is.na(J_old) && !is.nan(J_old)) {
      if (abs(J - J_old) < J_old * tol) {
        break
      }
    } else {
      warning("Invalid J or J_old encountered")
      break
    }
  }
  
  sort_norm <- sort(normdiff, decreasing = TRUE)
  
  if (type == 1) {
    a <- sort_norm[floor(n/2)]
    b <- 0
    c <- 0
  } else if (type == 2) {
    a <- sort_norm[floor(n/2)]
    b <- sort_norm[floor(n/20)]
    c <- max(normdiff)
  }
  
  return(c(a, b, c))
}

# Main RKDE function
robkde <- function(x, h, type = 2) {
  if (missing(type)) {
    type <- 2
  }
  
  n <- nrow(x)
  d <- ncol(x)
  
  # Construct kernel matrix (Gaussian kernel with bandwidth h)
  X <- as.matrix(dist(x)^2)
  K <- gauss_kern(X, h, d)
  
  # Find median absolute deviation
  params <- parameter_select(K, type)
  a <- params[1]
  b <- params[2]
  c <- params[3]
  
  # Initial weights
  w <- rep(1/n, n)
  tol <- 1e-8
  
  norm2mu <- sum(w * (K %*% w))
  normdiff <- sqrt(diag(K) + norm2mu - 2 * K %*% w)
  
  while(TRUE) {
    J_old <- sum(normdiff) / n
    w <- 1 / normdiff
    w <- w / sum(w)
    
    norm2mu <- sum(w * (K %*% w))
    normdiff <- sqrt(diag(K) + norm2mu - 2 * K %*% w)
    
    J <- sum(normdiff) / n
    
    if (abs(J - J_old) < J_old * tol) {
      break
    }
  }
  
  return(list(w = w, a = a, b = b, c = c))
}



############################################################

# Generate training data with mixture of Gaussian
d <- 1
n0 <- 200  # Nominal sample size
n1 <- 20   # Number of outliers
nc1 <- rbinom(1, n0, 0.4)
x1 <- c(rnorm(nc1, 3, 1), rnorm(n0 - nc1, 8, 1))

# Generate outliers with uniform distribution
x0 <- runif(n1, -5, 15)

# Combine data
x <- rbind(matrix(x1, ncol = d), matrix(x0, ncol = d))

# Bandwidth selection
b_type <- 1
h <- bandwidth_select(x, b_type)

# Weights for KDE and RKDE
w_kde <- rep(1/(n0 + n1), n0 + n1)

# Assume robkde function is defined
type <- 2  # Hampel loss
robkde_result <- robkde(x, h, type)
w_hm <- robkde_result$w

# Compute density estimates
y <- seq(-5, 15, by = 0.01)
m <- length(y)
Y <- outer(y, x[, 1], FUN = function(y, x) (y - x)^2)
pd <- gauss_kern(Y, h, d)
f_kde <- pd %*% w_kde
f_hm <- pd %*% w_hm

# True density
f_true <- 0.4 * dnorm(y, 3, 1) + 0.6 * dnorm(y, 8, 1)

# Plotting
plot(y, f_true, type = "l", lwd = 2, col = "black", ylim = c(0, 0.35))
lines(y, f_kde, col = "blue", lty = 2)
lines(y, f_hm, col = "red", lty = 3)
points(x1, rep(0.005, length(x1)), col = "blue", pch = 20)
points(x0, rep(0.005, length(x0)), col = "red", pch = 4)
legend("topright", legend = c("True", "KDE", "RKDE (Hampel)", "Nominal Sample", "Outliers"),
       col = c("black", "blue", "red", "blue", "red"), lty = c(1, 2, 3, NA, NA),
       pch = c(NA, NA, NA, 20, 4))

# Assuming IF function (Influence Function) is defined for RKDE
# Display influence function (IF)
# ...

# Note: Additional code is needed for displaying the influence function,
# which depends on the specific implementation of the IF function in R.
