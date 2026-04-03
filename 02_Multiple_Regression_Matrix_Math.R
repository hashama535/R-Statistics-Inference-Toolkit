# Title: Multiple Regression via Matrix Algebra
# Demonstrating: b = (X'X)^-1 * X'y

y <- c(57.5, 52.8, 61.3, 67, 53.5, 62.7, 56.2, 68.5, 69.2)
x1 <- c(78, 69, 77, 88, 67, 80, 74, 94, 102)
x2 <- c(2.75, 2.15, 4.41, 5.52, 3.21, 4.32, 2.31, 4.30, 3.71)
n <- length(y)
k <- 3 # Intercept + 2 Regressors

# Construct Design Matrix X
X <- cbind(1, x1, x2)

# OLS Formula Implementation
xtx <- t(X) %*% X
xty <- t(X) %*% y
xtx_inv <- solve(xtx)

b_hat <- xtx_inv %*% xty

# Results & Diagnostic
y_hat <- X %*% b_hat
residuals <- y - y_hat
sigma_hat_sq <- sum(residuals^2) / (n - k)

cat("Manual Coefficients (Intercept, x1, x2):\n")
print(round(b_hat, 4))
