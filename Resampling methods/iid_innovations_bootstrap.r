# Original AR(1) model parameters
true_coefficient <- 0.5
n_data_points <- 100
original_ar1_series <- arima.sim(model = list(ar = true_coefficient), n = n_data_points)

# Fit the original AR(1) model using OLS
original_ar1_model <- ar.ols(original_ar1_series, order.max = 1)

# True AR(1) coefficient
beta_hat <- original_ar1_model$ar[1]

# The residuals (innovations)
eps_hat <- original_ar1_model$resid[2:100]

# Compute the mean of residuals (innovations)
eps_bar <- mean(eps_hat)

# Compute centered residuals
eps_tilde <-  eps_hat - eps_bar

# Number of bootstrap samples
n_bootstrap <- 500
# Vector to store the bootstrap estimates of the Least Square estimate
bootstrap_T <- numeric(n_bootstrap)



# IID-innovation bootstrap
for (i in 1:n_bootstrap) {
  # Generate a bootstrap sample from the residuals
  bootstrap_residuals <- sample(eps_tilde, replace = TRUE)
  
  # Create bootstrap pseudo-observations
  bootstrap_series <- numeric(n_data_points)
  bootstrap_series[1] <- original_ar1_series[1]  # Initialize with the first data point
  
  for (t in 2:n_data_points) {
    bootstrap_series[t] <- beta_hat * bootstrap_series[t - 1] + bootstrap_residuals[t - 1]
  }
  
  # Fit an AR(1) model to the bootstrap sample using OLS
  bootstrap_ar1_model <- ar.ols(bootstrap_series, order.max = 1)
  bootstrap_beta_hat <- bootstrap_ar1_model$ar[1]
  # Calculate the bootstrap estimates (scaled beta)
  bootstrap_T[i]<-(bootstrap_beta_hat - beta_hat)*sqrt(sum(bootstrap_series[1:99]^2))
}

# Standardize the bootstrap estimates
std_bootstrap_estimates <- (bootstrap_T - mean(bootstrap_T, na.rm = TRUE)) / sd(bootstrap_T, na.rm = TRUE)
 


# Simulate the AR(1) model multiple times to create distribution of sample estimates
sample_T<-numeric(10000)
for (i in 1:10000) {
  simulated_data <- arima.sim(list(order = c(1, 0, 0), ar = true_coefficient), n = 100)
  sim_model <- ar.ols(simulated_data, order.max = 1)
  # Calculate the sample estimates (scaled beta)
  sample_T[i]<-(sim_model$ar[1]-true_coefficient)*(sqrt(sum(simulated_data[1:99]^2)))
}
# Standardize the sample estimates
std_sample_estimates <- (sample_T - mean(sample_T, na.rm = TRUE)) / sd(sample_T, na.rm = TRUE)

# Plot the ECDFs to compare the distributions
plot(ecdf(std_sample_estimates), main = "Bootstrap vs. True Distribution of Standardized Least Square Estimate")
lines(ecdf(std_bootstrap_estimates), col = "blue", cex = 0.2)
legend("bottomright", legend = c("Sample Estimates", "Bootstrap Estimates"), col = c("black", "blue"), lty = 1, cex = 0.8)
