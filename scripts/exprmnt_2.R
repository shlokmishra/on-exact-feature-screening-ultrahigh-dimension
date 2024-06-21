gauss_kernel <- function(dist, h, d) {
  # Calculate the Gaussian kernel values
  kernel_values <- exp(-dist / (2 * h^2)) / ((2 * pi * h^2)^(d / 2))
  return(kernel_values)
}

IF <- function(w, x, y, h, type, a, b, c) {
  # Get dimensions
  n <- nrow(x)
  d <- ncol(x)
  m <- nrow(y)
  
  # Construct squared distance matrices
  X <- matrix(0, n, n)
  Y <- matrix(0, m, n)
  for (i in 1:d) {
    X <- X + (matrix(1, n, 1) %*% t(x[, i]) - x[, i] %*% matrix(1, 1, n))^2
    Y <- Y + (matrix(1, m, 1) %*% t(y[, i]) - y[, i] %*% matrix(1, 1, n))^2
  }
  
  # Calculate Gaussian kernels
  K <- gauss_kernel(X, h, d)
  Ky <- gauss_kernel(Y, h, d)
  k0 <- gauss_kernel(0, h, d)
  
  # Additional calculations
  norm2mu <- t(w) %*% K %*% w
  r <- Re(sqrt(diag(K) + norm2mu - 2 * K %*% w))
  ry <- Re(sqrt(k0 + norm2mu - 2 * Ky %*% w))
  
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

psi_prime <- function(inp, type, a, b, c) {
  # Get dimensions
  n <- nrow(inp)
  m <- ncol(inp)
  
  # Initialize output
  out <- matrix(0, n, m)
  
  # Switch based on loss function type
  switch(type,
         "1" = {  # Huber function
           i1 <- inp <= a
           i2 <- inp > a
           out[i1] <- 1
           out[i2] <- 0
         },
         "2" = {  # Hampel function
           i1 <- inp < a
           i2 <- inp >= a & inp < b
           i3 <- inp >= b & inp < c
           i4 <- inp >= c
           
           out[i1] <- 1
           out[i2] <- 0
           out[i3] <- -a / (c - b)
           out[i4] <- 0
         }
  )
  
  return(out)
}

psi <- function(inp, type, a, b, c) {
  # Get dimensions
  n <- nrow(inp)
  m <- ncol(inp)
  
  # Initialize output
  out <- matrix(0, n, m)
  
  # Switch based on loss function type
  switch(type,
         "1" = {  # Huber function
           out <- pmin(inp, a)  # Apply minimum element-wise
         },
         "2" = {  # Hampel function
           i1 <- inp < a
           i2 <- inp >= a & inp < b
           i3 <- inp >= b & inp < c
           i4 <- inp >= c
           
           out[i1] <- inp[i1]
           out[i2] <- a
           out[i3] <- a * (c - inp[i3]) / (c - b)
           out[i4] <- 0
         }
  )
  
  return(out)
}

rho <- function(x, type, a, b, c) {
  # Get length of input
  n <- length(x)
  
  # Switch based on loss function type
  switch(type,
         "1" = {  # Huber
           in1 <- x <= a
           in2 <- x > a
           J <- sum(1/2 * x[in1]^2) + sum(a * (x[in2] - a) + 1/2 * a^2)
         },
         "2" = {  # Hampel
           in1 <- x <= a
           in2 <- x > a & x <= b
           in3 <- x > b & x <= c
           in4 <- x > c
           
           p <- -a / (c - b)
           q <- a * c / (c - b)
           r <- a * b - 1/2 * a^2 - 1/2 * p * b^2 - q * b
           temp <- rep(0, n)  # Initialize temporary vector
           temp[in1] <- 1/2 * x[in1]^2
           temp[in2] <- a * (x[in2] - a) + 1/2 * a^2
           temp[in3] <- 1/2 * p * x[in3]^2 + q * x[in3] + r
           temp[in4] <- 1/2 * p * c^2 + q * c + r
           J <- sum(temp)
         }
  )
  
  # Normalize by n
  J <- J / n
  
  return(J)
}

bandwidth_select <- function(x, b_type = 1, sigma = pracma::logspace(-2, 1)) {
  # x: n by d matrix
  # b_type: bandwidth type (1 -> lscv, 2 -> lkcv, 3 -> jakkola heuristic)
  # sigma: bandwidth array
  
  n <- nrow(x)
  d <- ncol(x)
  
    # Check if x has only one row
  if (n <= 1) {
    warning("Input data has only one row. Returning a default bandwidth value.")
    return(sigma[1]) # or any other default/pre-defined value
  }

  # Construct matrix of squared distances
  X <- as.matrix(dist(x))^2
  
  if (b_type == 1) {
    # Least squares cross validation
    Jmin <- Inf
    for (h in sigma) {
      K1 <- (4 * pi * h^2)^(-d / 2) * exp(-X / (4 * h^2))
      K2 <- (2 * pi * h^2)^(-d / 2) * (exp(-X / (2 * h^2)) - diag(n))
      J <- sum(K1) / (n^2) - 2 / (n * (n - 1)) * sum(K2)
      if (J < Jmin) {
        h_opt <- h
        Jmin <- J
      }
    }
  } else if (b_type == 2) {
    # Log-likelihood cross validation
    Jmax <- -Inf
    for (h in sigma) {
      K <- (2 * pi * h^2)^(-d / 2) * (exp(-X / (2 * h^2)) - diag(n))
      J <- (1 / n) * sum(log(rowSums(K) / (n - 1)))
      if (J > Jmax) {
        h_opt <- h
        Jmax <- J
      }
    }
  } else if (b_type == 3) {
    # Jakkola heuristics
    X[X == 0] <- Inf
    Y <- apply(X, 1, min)
    h_opt <- sqrt(median(Y))
  }
  
  return(h_opt)
}

robkde <- function(x, h, type = 2) {
  # Get dimensions
  n <- nrow(x)
  d <- ncol(x)
  

  # Construct kernel matrix (Gaussian kernel with bandwidth h)
  X <- matrix(0, n, n)  # Use matrix() for matrix creation
  for (i in 1:d) {
    X <- X + (t(rep(1, n)) %*% t(x[, i]) - x[, i] %*% t(rep(1, n)))^2
  }
  
  # Ensure h has a valid value before calling gauss_kernel
  if (is.null(h)) {
    # Handle missing h (e.g., provide a default value or modify gauss_kernel)
    stop("Missing bandwidth h. Please provide a value or adjust gauss_kernel.")
  }
  
  K <- gauss_kernel(X, h, d)
  

  # Find median absolute deviation
  a <- b <- c <- numeric(0)  # Initialize a, b, c
  if (type %in% c(1, 2)) {  # Ensure valid type
    temp <- parameter_select(K, type)
    a <- temp[1]
    b <- temp[2]
    c <- temp[3]
  } else {
    stop("Invalid loss function type specified.")
  }
  
  
  # Initial weights
  w <- rep(1/n, n)
  tol <- 10^-8
  
  norm2mu <- t(w) %*% K %*% w
  # normdiff <- Re(sqrt(diag(K) + norm2mu - 2 * K %*% w))  # Original line
  dim(K)
  dim(w)
  normdiff <- norm(K - 2 * K %*% w, "F")  # Alternative approach
  
  J <- rho(normdiff, type, a, b, c)
  
  while (TRUE) {
    J_old <- J
    w <- psi(normdiff, type, a, b, c) / normdiff
    w <- w / sum(w)
    
    norm2mu <- t(w) %*% K %*% w
    normdiff <- Re(sqrt(diag(K) + norm2mu - 2 * K %*% w))
    
    J <- rho(normdiff, type, a, b, c)
    if (abs(J - J_old) < J_old * tol) {
      break
    }
  }
  
  return(list(w = w, a = a, b = b, c = c))
}

parameter_select <- function(K, type) {
  # Get dimensions
  n <- nrow(K)
  
  # Initial weights (ensure numeric for potential matrix inputs)
  w <- as.numeric(rep(1/n, n))
  tol <- 10^-8
  
  # Calculate norm2mu and normdiff using Frobenius norm for robustness
  norm2mu <- norm(K %*% w, "F")^2
  normdiff <- norm(K - 2 * K %*% w, "F")
  
  # Iterate for convergence
  J <- normdiff / n
  while (TRUE) {
    J_old <- J
    w <- 1 / normdiff
    w <- w / sum(w)
    
    norm2mu <- norm(K %*% w, "F")^2
    normdiff <- norm(K - 2 * K %*% w, "F")
    
    J <- normdiff / n
    if (abs(J - J_old) < J_old * tol) {
      break
    }
  }
  
  # Select parameters based on type
  sort_norm <- sort(normdiff, decreasing = TRUE)
  if (type == 1) {
    a <- sort_norm[floor(n/2)]
    b <- 0
    c <- 0
  } else if (type == 2) {
    a <- sort_norm[floor(n/2)]
    b <- sort_norm[floor(n/20)]
    c <- max(normdiff)
  } else {
    stop("Invalid loss function type specified.")
  }
  
  return(c(a, b, c))
}

# Parameters
d <- 1
n0 <- 200
n1 <- 20

# Generate training data with mixture of Gaussians
nc1 <- rbinom(1, n0, 0.4)
x1 <- c(rnorm(nc1, 3, 1), rnorm(n0 - nc1, 8, 1))

# Generate outliers
x0 <- 20 * runif(n1) - 5

# Combine data
n <- n0 + n1
x <- c(x1, x0)

##To deal with nrow(x) = NULL
x <- matrix(x, nrow = 1)

# Bandwidth selection
b_type <- 1
h <- bandwidth_select(x, b_type, sigma = pracma::logspace(-2, 1))

# Weights for KDE and RKDE
w_kde <- rep(1/n, n)
type <- 2  # Hampel loss
result <- robkde(x, h, type)
w_hm <- result$w
a <- result$a
b <- result$b
c <- result$c

# Density estimates
y <- seq(-5, 15, 0.01)
m <- length(y)

# True density
f <- 0.4 * dnorm(y, 3, 1) + 0.6 * dnorm(y, 8, 1)

Y <- outer(y, x, FUN = function(y, x) (y - x)^2)  # Matrix of squared distances
pd <- gauss_kernel(Y, h, d)
f_kde <- pd %*% w_kde
f_hm <- pd %*% w_hm

# Plot density estimates
plot(y, f, type = "l", col = "black", lwd = 2, ylim = c(0, 0.35), xlab = "y", ylab = "Density")
lines(y, f_kde, lty = 2, col = "blue")
lines(y, f_hm, lty = 3, col = "red")
points(x1, rep(0.005, length(x1)), pch = 19, col = "blue", cex = 0.6)
points(x0, rep(0.005, length(x0)), pch = 4, col = "red", cex = 0.6)
legend("topright", legend = c("True", "KDE", "RKDE(Hampel)", "Nominal", "Outliers"),
       col = c("black", "blue", "red", "blue", "red"), lty = c(1, 2, 3, NA, NA), pch = c(NA, NA, NA, 19, 4))

# Influence function
x_prime <- c(-3, 2, 7, 12)
l <- length(x_prime)

alpha_kde <- c(-1/n * rep(1, n), 1)
alpha_hm <- IF(w_hm, x, x_prime, h, 2, a, b, c)

par(mfrow = c(2, 2))
for (i in 1:l) {
  x_extend <- c(x, x_prime[i])
  Z <- outer(y, x_extend, FUN = function(y, x) (y - x)^2)
  pd2 <- gauss_kernel(Z, h, d)
  IF_KDE <- pd2 %*% alpha_kde
  IF_HM <- pd2 %*% alpha_hm[, i]
  
  plot(y, IF_KDE, type = "l", col = "blue", ylim = c(-0.15, 0.4))
  lines(y, IF_HM, lty = 3, col = "red")
  points(x_prime[i], 0.005, pch = 19, col = "blue", cex = 0.
         