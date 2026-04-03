# Title: Manual Statistical Inference & Intervals
# Focus: Hypothesis Testing and Research Precision

x <- c(56, 48, 42, 58, 40, 39, 50)
y <- c(45, 38.5, 34.5, 46.1, 33.3, 32.1, 40.2)
n <- length(x)

# Manual Parameter Estimation
b <- (n*sum(x*y) - (sum(x)*sum(y))) / (n*sum(x^2) - (sum(x)^2))
a <- mean(y) - (b * mean(x))

# Standard Error and T-Critical Value
se <- sqrt((sum(y^2) - (a*sum(y)) - (b*sum(x*y))) / (n-2))
t_crit <- qt(0.975, df = n - 2)

# Calculation at X0 = 44
x0 <- 44
y_hat0 <- a + b * x0
sxx <- sum((x - mean(x))^2)

# Inference Intervals
se_mean <- se * sqrt((1/n) + ((x0 - mean(x))^2 / sxx))
se_pred <- se * sqrt(1 + (1/n) + ((x0 - mean(x))^2 / sxx))

cat("95% Confidence Interval: [", y_hat0 - t_crit*se_mean, ",", y_hat0 + t_crit*se_mean, "]\n")
cat("95% Prediction Interval: [", y_hat0 - t_crit*se_pred, ",", y_hat0 + t_crit*se_pred, "]")
