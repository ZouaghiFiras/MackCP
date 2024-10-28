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
  expect_type(results$ultimate, "numeric", info = "'ultimate' claims should be numeric.")
  expect_type(results$standard_error, "numeric", info = "'standard_error' should be numeric.")

  # Check that the ultimate claims are positive
  expect_true(all(results$ultimate >= 0), info = "'ultimate' claims should be non-negative.")

  # Check that the standard errors are non-negative
  expect_true(all(results$standard_error >= 0), info = "'standard_error' should be non-negative.")
})

test_that("mack_estimator handles invalid input gracefully", {
  # Test with invalid data (e.g., empty data frame)
  empty_data <- data.frame()

  expect_error(mack_estimator(empty_data),
               "Data must be a valid claims triangle.",
               info = "The function should throw an error for empty input data.")

  # Test with NA values in data
  invalid_data <- data.frame(Development = c(1, 2, NA),
                              Origin = c(1, 1, 1),
                              Claims = c(100, 200, 300))

  expect_warning(mack_estimator(invalid_data),
                 "Data contains NA values.",
                 info = "The function should issue a warning for NA values in input data.")
})

test_that("mack_estimator compares correctly with ChainLadder", {
  # Load the RAA dataset
  data("RAA", package = "ChainLadder")

  # Get results from MackCP's mack_estimator
  mackcp_results <- mack_estimator(RAA)

  # Get results from ChainLadder's MackChainLadder
  chainladder_results <- MackChainLadder(RAA, est.sigma = "Mack")

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
