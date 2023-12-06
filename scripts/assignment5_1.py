import numpy as np

# Define the number of simulations
n_simulations = 1000000  # Feel free to change this number based on your computational resources

# Generate n_simulations random samples from a standard normal distribution
samples = np.random.normal(0, 1, n_simulations)

# Compute the expected value of cos^2(X) using Monte Carlo simulation
expected_value = np.mean(np.cos(samples)**2)

# Calculate the standard error to quantify the uncertainty of the estimate
standard_error = np.std(np.cos(samples)**2) / np.sqrt(n_simulations)

# Print the results
print(f"Estimated mean value E[cos^2(X)]: {expected_value}")
print(f"Standard error of the estimate: {standard_error}")
