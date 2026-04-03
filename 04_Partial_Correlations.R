# Title: Partial Correlation Analysis
# Use Case: Controlling for confounding variables in research

y  <- c(57.5, 52.8, 61.3, 67, 53.5, 62.7, 56.2, 68.5, 69.2)
x1 <- c(78, 69, 77, 88, 67, 80, 74, 94, 102)
x2 <- c(2.75, 2.15, 4.41, 5.52, 3.21, 4.32, 2.31, 4.30, 3.71)

# Simple Correlations
r12 <- cor(y, x1)
r13 <- cor(y, x2)
r23 <- cor(x1, x2)

# Partial Correlation r12.3 (Y and X1 controlling for X2)
r12_3 <- (r12 - r13*r23) / sqrt((1 - r13^2)*(1 - r23^2))

cat("The Partial Correlation (r12.3) is:", round(r12_3, 4))
