# Load necessary libraries
library(testthat)
library(MackCP)
library(ChainLadder)

# Define test cases for the mack_estimator function
test_that("mack_estimator returns expected results", {
  # Load the RAA dataset
  data("RAA", package = "ChainLadder")
  
  # Run the mack_estimator function on the RAA dataset
  results <- mack_estimator(RAA)
  
  # Check that results contain the expected elements
  expect_true("ultimate" %in% names(results), info = "Results should include 'ultimate' claims.")
  expect_true("standard_error" %in% names(results), info = "Results should include 'standard_error'.")
  
  # Validate ultimate claims and standard errors
  expect_type(results$ultimate, "numeric", info = "'ultimate' should be numeric.")
  expect_type(results$standard_error, "numeric", info = "'standard_error' should be numeric.")
  
  # Check that the ultimate claims are positive
  expect_true(all(results$ultimate >= 0), info = "'ultimate' should be non-negative.")
  
  # Check that the standard errors are non-negative
  expect_true(all(results$standard_error >= 0), info = "'standard_error' should be non-negative.")
})

test_that("mack_estimator handles invalid input gracefully", {
  # Test with invalid data (e.g., empty data frame)
  empty_data <- data.frame()
  
  expect_error(mack_estimator(empty_data),
               "The input 'triangle' must be a matrix or a data frame.",
               info = "The function should throw an error for empty input data.")
  
  # Test with NA values in data
  invalid_data <- matrix(c(1, 2, NA, 1, 1, 100, 200, 300), nrow = 3, byrow = TRUE)
  
  expect_error(mack_estimator(invalid_data),
               "Data contains NA values.",
               info = "The function should throw an error for NA values in input data.")
  
  # Test with non-square matrix
  non_square_matrix <- matrix(1:6, nrow = 2, ncol = 3)
  expect_error(mack_estimator(non_square_matrix),
               "Input triangle must be a square or upper triangular matrix.",
               info = "The function should throw an error for non-square matrix.")
})

test_that("mack_estimator compares correctly with ChainLadder", {
  # Load the RAA dataset
  data("RAA", package = "ChainLadder")
  
  # Get results from MackCP's mack_estimator
  mackcp_results <- mack_estimator(RAA)
  
  # Get results from ChainLadder's MackChainLadder
  chainladder_results <- ChainLadder::MackChainLadder(RAA, est.sigma = "Mack")
  
  # Compare ultimate claims
  expect_equal(mackcp_results$ultimate,
               chainladder_results$FullTriangle[, ncol(chainladder_results$FullTriangle)],
               tolerance = 0.01,
               info = "Ultimate claims from MackCP should match those from ChainLadder within a tolerance of 0.01.")
  
  # Compare standard errors
  expect_equal(mackcp_results$standard_error,
               chainladder_results$Mack.S.E,
               tolerance = 0.01,
               info = "Standard errors from MackCP should match those from ChainLadder within a tolerance of 0.01.")
})
