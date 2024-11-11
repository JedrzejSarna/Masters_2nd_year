setwd("/Users/jedrzejsarna/Desktop/II stopien 4 semestr/Statystyczna Analiza Danych Biomedycznych")

# Load necessary libraries
library(readxl)
library(dplyr)

# Load the data from the provided Excel file
data <- read_excel("Dane 11.1.xlsx")

# Assuming the data has columns: G (grouping variable), PW (PW variable), D (binary indicator), and T (some relevant variable)
# Adjust column names based on actual data if different

# Define the log-likelihood function for the unrestricted model
log_likelihood <- function(beta, data) {
  # Extract variables from data
  G <- data$G
  PW <- data$PW
  D <- data$D
  T <- data$T
  X <- as.matrix(cbind(G, PW))
  
  # Compute the sum part of the log-likelihood
  sum_part <- function(i) {
    if (D[i] == 1) {
      sum_beta_xi <- sum(beta * X[i,])
      log_sum_exp <- log(sum(exp(X %*% beta)[T >= T[i]]))
      return(sum_beta_xi - log_sum_exp)
    } else {
      return(0)
    }
  }
  
  # Apply the sum part function to all rows
  sum_values <- sapply(1:nrow(data), sum_part)
  
  # Return the log-likelihood
  return(sum(sum_values))
}

# Define the log-likelihood function for the restricted model
log_likelihood_constrained <- function(beta, data) {
  # Extract variables from data
  G <- data$G
  PW <- data$PW
  D <- data$D
  T <- data$T
  X <- as.matrix(cbind(G, PW))
  
  # Setting beta_l = 0 (let's assume beta[1] is beta_l and set it to 0)
  beta[2] <- 0
  
  # Compute the sum part of the log-likelihood
  sum_part <- function(i) {
    if (D[i] == 1) {
      sum_beta_xi <- sum(beta * X[i,])
      log_sum_exp <- log(sum(exp(X %*% beta)[T >= T[i]]))
      return(sum_beta_xi - log_sum_exp)
    } else {
      return(0)
    }
  }
  
  # Apply the sum part function to all rows
  sum_values <- sapply(1:nrow(data), sum_part)
  
  # Return the log-likelihood
  return(sum(sum_values))
}

# Define a grid search function
grid_search <- function(log_likelihood_func, data, beta_range, step_size) {
  best_beta <- c(NA, NA)
  best_log_likelihood <- -Inf
  
  for (beta1 in seq(beta_range[1], beta_range[2], by = step_size)) {
    for (beta2 in seq(beta_range[1], beta_range[2], by = step_size)) {
      beta <- c(beta1, beta2)
      ll <- log_likelihood_func(beta, data)
      if (ll > best_log_likelihood) {
        best_log_likelihood <- ll
        best_beta <- beta
      }
    }
  }
  
  return(list(beta = best_beta, log_likelihood = best_log_likelihood))
}

# Define the range and step size for the grid search
beta_range <- c(-2, 2)  # Adjust the range as needed
step_size <- 0.1  # Adjust the step size as needed

# Perform the grid search for the unrestricted model
result_unrestricted <- grid_search(log_likelihood, data, beta_range, step_size)

# Perform the grid search for the restricted model
result_restricted <- grid_search(log_likelihood_constrained, data, beta_range, step_size)

# Extract the estimated beta and log-likelihoods
beta_est_unrestricted <- result_unrestricted$beta
log_likelihood_unrestricted <- result_unrestricted$log_likelihood

beta_est_restricted <- result_restricted$beta
log_likelihood_restricted <- result_restricted$log_likelihood

# Compute the test statistic T
T <- 2 * (log_likelihood_unrestricted - log_likelihood_restricted)

# Print the estimated betas and test statistic
print(beta_est_unrestricted)
print(log_likelihood_unrestricted)
print(beta_est_restricted)
print(log_likelihood_restricted)
print(T)


# MOZNA UZYWAC FUNKCJI OPTIM !!!