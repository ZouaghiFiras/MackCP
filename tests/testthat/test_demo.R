# Load necessary libraries
library(testthat)
library(MackCP)
library(ChainLadder)

# Load RAA dataset
data("RAA", package = "ChainLadder")
print(head(RAA))  # Check if the dataset is loaded correctly

# Run MackCP's estimator
mackcp_results <- mack_estimator(RAA)
print(mackcp_results)  # Inspect the structure

# Run ChainLadder's Mack estimator
chainladder_results <- ChainLadder::MackChainLadder(RAA, est.sigma = "Mack")
print(chainladder_results)  # Inspect the structure

# Compare ultimate claims
expect_equal(mackcp_results$ultimate, chainladder_results$FullTriangle[, ncol(chainladder_results$FullTriangle)], tolerance = 0.01)

# Compare standard errors
expect_equal(mackcp_results$standard_error, chainladder_results$Mack.S.E, tolerance = 0.01)
