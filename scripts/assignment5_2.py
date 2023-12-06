import numpy as np
import scipy.stats as stats

# Define the number of simulations
n_simulations = 10000  # You can adjust this number based on desired accuracy and computational capacity

# Observed data
observed_correlation = 0.3
n_observations = 10  # Number of experiments conducted

# Record the number of times the simulated correlation exceeds the observed correlation
count_exceeds_observed = 0

# Perform Monte Carlo simulations
for _ in range(n_simulations):
    # Generate two sets of random data for the hyperparameter A and score S
    A = np.random.normal(0, 1, n_observations)
    S = np.random.normal(0, 1, n_observations)
    
    # Compute the correlation coefficient for the simulated data
    simulated_correlation = stats.pearsonr(A, S)[0]
    
    # Check if the absolute value of the simulated correlation is greater than the observed
    if abs(simulated_correlation) >= observed_correlation:
        count_exceeds_observed += 1

# Compute the simulated p-value
simulated_p_value = count_exceeds_observed / n_simulations

# Print the simulated p-value
print(f"Simulated p-value: {simulated_p_value}")
