# Load necessary libraries
library(testthat)
library(MackCP)
library(ChainLadder)

# Define test cases for the comparison between MackCP and ChainLadder Mack's estimator results

test_that("Comparison between MackCP and ChainLadder results is consistent", {
  # Load the RAA dataset from ChainLadder
  data("RAA", package = "ChainLadder")

  # Step 1: Use MackCP's mack_estimator function to calculate ultimate claims and standard errors
  mackcp_results <- mack_estimator(RAA)
  mackcp_ultimate <- mackcp_results$ultimate
  mackcp_se <- mackcp_results$standard_error

  # Step 2: Use ChainLadder's MackChainLadder function to calculate ultimate claims and standard errors
  chainladder_results <- MackChainLadder(RAA, est.sigma = "Mack")
  chainladder_ultimate <- chainladder_results$FullTriangle[, ncol(chainladder_results$FullTriangle)]
  chainladder_se <- chainladder_results$Mack.S.E

  # Step 3: Compare the ultimate claims from both methods
  expect_equal(mackcp_ultimate, chainladder_ultimate, tolerance = 0.01,
               info = "Ultimate claims from MackCP and ChainLadder should be similar within a tolerance of 0.01")

  # Step 4: Compare the standard errors from both methods
  expect_equal(mackcp_se, chainladder_se, tolerance = 0.01,
               info = "Standard errors from MackCP and ChainLadder should be similar within a tolerance of 0.01")
})

# Additional tests to cover edge cases, if needed
test_that("MackCP handles empty or malformed datasets gracefully", {
  # Test with an empty dataset
  empty_triangle <- matrix(numeric(0), nrow = 0, ncol = 0)
  expect_error(mack_estimator(empty_triangle),
               "Input triangle must have at least one row and one column")

  # Test with a non-square matrix (to ensure the input triangle shape is valid)
  invalid_triangle <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  expect_error(mack_estimator(invalid_triangle),
               "Input triangle must be a square or upper triangular matrix")
})
