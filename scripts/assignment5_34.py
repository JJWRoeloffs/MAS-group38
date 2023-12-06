import numpy as np
from scipy import stats
# Parameters for the normal distributions
mu, sigma = 0, 1  # Mean and variance for f
nu, tau = 1, 2    # Mean and variance for g

# Theoretical KL divergence between two normal distributions
theoretical_kl_divergence = np.log(tau/sigma) + (sigma**2 + (mu - nu)**2) / (2*tau**2) - 0.5

# Number of samples for the Monte Carlo simulation
n_samples = 1000000  # Adjust this as needed

# Sample from the normal distribution f
samples_f = np.random.normal(mu, sigma, n_samples)

# Calculate the sample-based estimate of the KL-divergence
sample_kl_divergence = np.mean(np.log(stats.norm.pdf(samples_f, mu, sigma) / stats.norm.pdf(samples_f, nu, tau)))

# Print the theoretical and sample-based KL divergence
print(f"Theoretical KL divergence: {theoretical_kl_divergence}")
print(f"Sample-based KL divergence: {sample_kl_divergence}")
