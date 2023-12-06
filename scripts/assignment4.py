import numpy as np

payoff_matrix = np.array([[[1, 5], [2, 2], [3, 4], [3, 1]],
                          [[3, 0], [4, 1], [2, 5], [4, 2]],
                          [[1, 3], [2, 6], [5, 2], [2, 3]]])

# Define the number of iterations
num_iterations = 1000

# Initialize the probability distributions for each player
player1_distribution = np.array([1/3, 1/3, 1/3])
player2_distribution = np.array([1/4, 1/4, 1/4, 1/4])

# Run the fictitious play algorithm
for i in range(num_iterations):
    # Calculate the expected payoffs for each player
    player1_expected_payoffs = np.dot(payoff_matrix[:, :, 1], player2_distribution)
    player2_expected_payoffs = np.dot(payoff_matrix[:, :, 0].T, player1_distribution)

    # Update the probability distributions for each player
    player1_distribution = (player1_distribution * (i / (i + 1)) + (1 / (i + 1)) * np.eye(1, 3, np.argmax(player1_expected_payoffs))).reshape(-1)
    player2_distribution = (player2_distribution * (i / (i + 1)) + (1 / (i + 1)) * np.eye(1, 4, np.argmax(player2_expected_payoffs))).reshape(-1)

# Print the final probability distributions for each player
print("Player 1 distribution: {}".format(player1_distribution))
print("Player 2 distribution: {}".format(player2_distribution))
