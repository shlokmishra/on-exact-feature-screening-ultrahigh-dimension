# Function to calculate the Gaussian kernel
# K: Gaussian kernel
# dist: matrix of squared distances
# h: bandwidth
# d: dimension
gauss_kern <- function(dist, h, d) {
  K <- exp(-dist / (2 * h^2)) / ((2 * pi * h^2)^(d / 2))
  return(K)
}


# Function to calculate alpha - weights for influence function
# alpha: weights for influence function
# w: weights for RKDE
# x: training data
# y: corresponds to x'
# h: bandwidth
# type: type of loss function, 1-> Huber, 2-> Hampel
# a, b, c: parameters
IF <- function(w, x, y, h, type, a, b, c) {
  n <- dim(x)[1]
  d <- dim(x)[2]
  m <- dim(y)[1]
  
  
  # Construct matrix of squared distances
  X <- matrix(0, n, n)
  Y <- matrix(0, m, n)
  
  for (i in 1:d) {
    X <- X + (matrix(1, n, 1) %*% t(x[, i]) - x[, i] %*% matrix(1, 1, n))^2
  }
  
  for (i in 1:d) {
    Y <- Y + (matrix(1, m, 1) %*% t(x[, i]) - y[, i] %*% matrix(1, 1, n))^2
  }
  
  K <- gauss_kern(X, h, d)
  Ky <- gauss_kern(Y, h, d)
  k0 <- gauss_kern(0, h, d)
  
  norm2mu <- sum(w * K %*% w)
  r <- sqrt(diag(K) + norm2mu - 2 * K %*% w)
  ry <- sqrt(k0 + norm2mu - 2 * Ky %*% w)
  
  gamma <- sum(psi(r, type, a, b, c) / r)
  
  g <- (r * psi_prime(r, type, a, b, c) - psi(r, type, a, b, c)) / (r^3)
  
  A <- matrix(1, n, 1) %*% t(w) - diag(n)
  B <- t(A) %*% diag(g) %*% A
  C <- gamma * diag(n) + B %*% K
  
  z <- (psi(ry, type, a, b, c) / ry) * n
  uy <- z / gamma
  
  D <- -w %*% t(z) - B %*% t(Ky) %*% diag(uy)
  u <- solve(C, D)
  alpha <- rbind(u, t(uy))
  
  return(alpha)
}



# Function to calculate psi function
# out: psi function evaluated at in
# in: input
# type: type of loss function, 1-> Huber, 2-> Hampel
# a, b, c: parameters
psi <- function(input, type, a, b, c) {
  n <- dim(input)[1]
  m <- dim(input)[2]
  out <- matrix(0, n, m)
  
  switch(type,
         1, {
           out <- pmin(input, a)
         },
         2, {
           i1 <- (input < a)
           i2 <- (input >= a & input < b)
           i3 <- (input >= b & input < c)
           i4 <- (input >= c)
           
           out[i1] <- input[i1]
           out[i2] <- a
           out[i3] <- a * (c - input[i3]) / (c - b)
           out[i4] <- 0
         }
  )
  
  return(out)
}



# Function to calculate psi' function
# out: psi' function evaluated at in
# in: input
# type: type of loss function, 1-> Huber, 2-> Hampel
# a, b, c: parameters
psi_prime <- function(input, type, a, b, c) {
  n <- dim(input)[1]
  m <- dim(input)[2]
  out <- matrix(0, n, m)
  
  switch(type,
         1, {
           i1 <- (input <= a)
           i2 <- (input > a)
           out[i1] <- 1
           out[i2] <- 0
         },
         2, {
           i1 <- (input < a)
           i2 <- (input >= a & input < b)
           i3 <- (input >= b & input < c)
           i4 <- (input >= c)
           
           out[i1] <- 1
           out[i2] <- 0
           out[i3] <- -a / (c - b)
           out[i4] <- 0
         }
  )
  
  return(out)
}


# Function to calculate J - a measure based on rho function
# J: (1/n) * sum_{i=1}^n rho(x_i)
# x: input
# type: type of loss function, 1-> Huber, 2-> Hampel
# a, b, c: parameters
rho <- function(x, type, a, b, c) {
  n <- length(x)
  
  switch(type,
         1, {
           # Huber
           in1 <- (x <= a)
           in2 <- (x > a)
           J <- sum(1/2 * x[in1]^2) + sum(a * (x[in2] - a) + 1/2 * a^2)
           print(paste("Calculated J for type 1:", J))  # Debugging line
         },
         2, {
           # Other calculations for type 2
         }
  )
  
  J <- J / n
  return(J)
}


# rho <- function(x, type, a, b, c) {
#   n <- length(x)
#   
#   switch(type,
#          1, {
#            # Huber
#            in1 <- (x <= a)
#            in2 <- (x > a)
#            J <- sum(1/2 * x[in1]^2) + sum(a * (x[in2] - a) + 1/2 * a^2)
#          },
#          2, {
#            # Hampel
#            in1 <- (x <= a)
#            in2 <- (a < x & x <= b)
#            in3 <- (b < x & x <= c)
#            in4 <- (c < x)
#            
#            p <- -a / (c - b)
#            q <- a * c / (c - b)
#            r <- a * b - 1/2 * a^2 - 1/2 * p * b^2 - q * b
#            
#            temp <- numeric(length(x))
#            temp[in1] <- 1/2 * x[in1]^2
#            temp[in2] <- a * (x[in2] - a) + 1/2 * a^2
#            temp[in3] <- 1/2 * p * x[in3]^2 + q * x[in3] + r
#            temp[in4] <- 1/2 * p * c^2 + q * c + r
#            
#            J <- sum(temp)
#          }
#   )
#   
#   J <- J / n
#   return(J)
# }

# Function to calculate RKDE weights, and parameters a, b, c
# w: weights for RKDE
# a, b, c: parameters
# x: n by d data matrix
# h: bandwidth
# type: type of loss function, 1-> Huber, 2-> Hampel
robkde <- function(x, h, type) {
  if (missing(type)) {
    type <- 2
  }
  
  dim_x <- dim(x)
  n <- dim_x[1]
  d <- dim_x[2]
  
  # Construct kernel matrix (Gaussian kernel with bandwidth h)
  X <- matrix(0, n, n)
  for (i in 1:d) {
    X <- X + (matrix(1, n, 1) %*% t(x[, i]) - x[, i] %*% matrix(1, 1, n))^2
  }
  K <- gauss_kern(X, h, d)
  
  # Find parameters a, b, c
  param <- parameter_select(K, type)
  a <- param[1]
  b <- param[2]
  c <- param[3]
  
  # Initialize weights
  w <- rep(1/n, n)
  tol <- 1e-8
  
  norm2mu <- t(w) %*% K %*% w
  normdiff <- sqrt(diag(K) + norm2mu - 2 * K %*% w)
  
  J <- rho(normdiff, type, a, b, c)
  
  while (TRUE) {
    J_old <- J
    w <- psi(normdiff, type, a, b, c) / normdiff
    w <- w / sum(w)
    
    norm2mu <- t(w) %*% K %*% w
    normdiff <- sqrt(diag(K) + norm2mu - 2 * K %*% w)
    
    J <- rho(normdiff, type, a, b, c)
    
    if (abs(J - J_old) < J_old * tol) {
      break
    }
  }
  
  return(list(w = w, a = a, b = b, c = c))
}

# Function to select parameters a, b, c based on the kernel matrix K
# a, b, c: parameters
# K: kernel matrix
parameter_select <- function(K, type) {
  # Find median
  n <- dim(K)[1]
  w <- rep(1/n, n)
  tol <- 1e-8
  
  norm2mu <- t(w) %*% K %*% w
  normdiff <- sqrt(diag(K) + norm2mu - 2 * K %*% w)
  
  J <- sum(normdiff) / n
  
  while (TRUE) {
    J_old <- J
    w <- 1 / normdiff
    w <- w / sum(w)
    
    norm2mu <- t(w) %*% K %*% w
    normdiff <- sqrt(diag(K) + norm2mu - 2 * K %*% w)
    
    J <- sum(normdiff) / n
    
    if (abs(J - J_old) < J_old * tol) {
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


# Function to select the optimal bandwidth
# h_opt: optimal bandwidth
# x: n by d vector
# b_type: bandwidth type: 1 -> lscv, 2 -> lkcv, 3 -> jakkola heuristic
# sigma: bandwidth array
bandwidth_select <- function(x, b_type = 1, sigma = 10^seq(-2, 1)) {
  l <- length(sigma)
  dim_x <- dim(x)
  n <- dim_x[1]
  d <- dim_x[2]
  
  # Construct matrix of squared distances
  X <- matrix(0, n, n)
  for (i in 1:d) {
    X <- X + (matrix(1, n, 1) %*% t(x[, i]) - x[, i] %*% matrix(1, 1, n))^2
  }
  
  if (b_type == 1) {
    # Least squares cross-validation
    Jmin <- Inf
    for (i in 1:l) {
      h <- sigma[i]
      K1 <- (4 * pi * h^2)^(-d/2) * exp(-X / (4 * h^2))
      K2 <- (2 * pi * h^2)^(-d/2) * (exp(-X / (2 * h^2)) - diag(n))
      J <- sum(sum(K1)) / (n^2) - 2 / (n * (n - 1)) * sum(sum(K2))
      if (J < Jmin) {
        h_opt <- h
        Jmin <- J
      }
    }
  } else if (b_type == 2) {
    # Log-likelihood cross-validation
    Jmax <- -Inf
    for (i in 1:l) {
      h <- sigma[i]
      K <- (2 * pi * h^2)^(-d/2) * (exp(-X / (2 * h^2)) - diag(n))
      J <- 1/n * sum(log(rowSums(K) / (n - 1)))
      if (J > Jmax) {
        h_opt <- h
        Jmax <- J
      }
    }
  } else if (b_type == 3) {
    # Jakkola heuristic
    # Include only distinct data points
    index <- (X == 0)
    X[index] <- Inf
    Y <- min(X)
    h_opt <- sqrt(median(Y))
  }
  
  return(h_opt)
}


# Written by JooSeuk Kim (2011/07/12)
library(MASS)  # You might need this library for normpdf function
library(stats)

# Clear all and close all are not necessary in R


# Set d, n0, and n1
d <- 1
n0 <- 200  # Number of nominal sample size
n1 <- 20   # Number of outliers

# Generate training data with a mixture of Gaussian distributions
nc1 <- rbinom(n0, 1, 0.4)
x1 <- matrix(c(rnorm(nc1, 1) + 3, rnorm(n0 - nc1, 1) + 8), ncol = 1)

# Generate outliers with a uniform distribution from -5 to 15
x0 <- matrix(20 * runif(n1) - 5, ncol = 1)

# Calculate the total number of samples
n <- n0 + n1

# Combine nominal and outlier data
x <- matrix(c(x1, x0), ncol = 1)

# Specify bandwidth type and calculate bandwidth
b_type <- 1  # Bandwidth type: 1 -> lscv, 2 -> lkcv, 3 -> jakkola heuristic
h <- bandwidth_select(x, b_type)

# Calculate weights for KDE
w_kde <- rep(1/n, n)

# Specify the type for RKDE (Hampel loss)
type <- 2

# Calculate weights and parameters for RKDE
res_robkde <- robkde(x, h, type)
w_hm <- res_robkde$w
a <- res_robkde$a
b <- res_robkde$b
c <- res_robkde$c

# Define the range of y values
y <- seq(-5, 15, by = 0.01)
y_col <- matrix(y, ncol = 1)

# Calculate true density
f <- 0.4 * dnorm(y, 3, 1) + 0.6 * dnorm(y, 8, 1)

# Calculate squared distances
# Y <- ((matrix(1, length(y), 1) %*% t(x)) - (y %*% matrix(1, 1, n)))^2
# Y <- (t(x) - y_col)^2
# Calculate squared distances
Y <- outer(y, t(x), FUN = function(yi, xi) (yi - xi)^2)

# Calculate the Gaussian kernel
pd <- gauss_kern(Y, h, d)

# Calculate density estimates for KDE and RKDE
f_kde <- pd %*% t(w_kde)
f_hm <- pd %*% t(w_hm)

# Plot the density estimates
plot(y, f, 'k-')
lines(y, f_kde, col = 'blue', lty = 2)
lines(y, f_hm, col = 'red', lty = 4)

# Plot the data points
points(x1, 0.005 * rep(1, length(x1)), pch = 'bo', cex = 0.6)
points(x0, 0.005 * rep(1, length(x0)), pch = 'rx', cex = 0.6)

# Add legend and adjust axis limits
legend('topright', legend = c('true', 'KDE', 'RKDE(Hampel)', 'nominal sample', 'outliers'))
axis(c(-5, 15, 0, 0.35))

# Display influence function
x_prime <- c(-3, 2, 7, 12)
l <- length(x_prime)

alpha_kde <- c(-1/n * rep(1, n), rep(1, 1))
alpha_hm <- matrix(0, n, l)

for (i in 1:l) {
  x_ext <- c(x, x_prime[i])
  Z <- (matrix(1, length(y), 1) %*% t(x_ext) - y %*% matrix(1, 1, n+1))^2
  pd2 <- gauss_kern(Z, h, d)
  IF_KDE <- pd2 %*% alpha_kde
  IF_HM <- pd2 %*% alpha_hm[, i]
  plot(y, IF_KDE, col = 'blue')
  lines(y, IF_HM, col = 'red', lty = 4)
  points(x_prime[i], 0.005, pch = 'bo', cex = 0.6)
  legend('topright', legend = c('KDE', 'RKDE(Hampel)', 'x`'))
}

# Assuming 'matIn' contains your MATLAB functions as character strings
matIn <-  "/home/shlok/code/rkde_code/demo.m"

# Using mat2r to convert, assuming matconv is loaded and configured
converted <- mat2r(matIn, verbose = 2)

# Retrieve and inspect the converted R code
convertedRCode <- converted$rCode
convertedRCode
