library(testthat)
library(MackCP)
library(ChainLadder)

# Load the RAA dataset from ChainLadder
data("RAA", package = "ChainLadder")

# Define test cases for the comparison between MackCP and ChainLadder Mack's estimator results
test_that("Comparison between MackCP and ChainLadder results is consistent", {
  # Step 1: Use MackCP's mack_estimator function to calculate ultimate claims and standard errors
  mackcp_results <- mack_estimator(RAA)
  
  # Check if the results contain required elements
  expect_true("ultimate" %in% names(mackcp_results), 
              info = "MackCP results should include 'ultimate' claims.")
  expect_true("standard_error" %in% names(mackcp_results), 
              info = "MackCP results should include 'standard_error'.")
  
  mackcp_ultimate <- mackcp_results$ultimate
  mackcp_se <- mackcp_results$standard_error
  
  # Step 2: Use ChainLadder's MackChainLadder function to calculate ultimate claims and standard errors
  chainladder_results <- ChainLadder::MackChainLadder(RAA, est.sigma = "Mack")
  chainladder_ultimate <- chainladder_results$FullTriangle[, ncol(chainladder_results$FullTriangle)]
  chainladder_se <- chainladder_results$Mack.S.E
  
  # Step 3: Compare the ultimate claims from both methods
  expect_equal(mackcp_ultimate, chainladder_ultimate, tolerance = 0.01,
               info = "Ultimate claims from MackCP and ChainLadder should be similar within a tolerance of 0.01")
  
  # Step 4: Compare the standard errors from both methods
  expect_equal(mackcp_se, chainladder_se, tolerance = 0.01,
               info = "Standard errors from MackCP and ChainLadder should be similar within a tolerance of 0.01")
})

# Additional tests to cover edge cases
test_that("MackCP handles empty or malformed datasets gracefully", {
  # Test with an empty dataset
  empty_triangle <- matrix(numeric(0), nrow = 0, ncol = 0)
  expect_error(mack_estimator(empty_triangle),
               "Input triangle must have at least one row and one column",
               info = "Expected error message for empty input.")
  
  # Test with a non-square matrix (to ensure the input triangle shape is valid)
  invalid_triangle <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  expect_error(mack_estimator(invalid_triangle),
               "Input triangle must be a square or upper triangular matrix",
               info = "Expected error message for invalid input.")
})

# Demo tests (if demo exists, remove if not needed)
test_that("Demo runs without errors", {
  expect_silent(demo("MackCP"), 
                info = "Demo should run without errors if it exists.")
})

test_that("Demo results are as expected", {
  # Example check: replace with actual checks if applicable
  mackcp_demo_results <- mack_estimator(RAA) # Call the estimator again for demo results
  expect_true("ultimate" %in% names(mackcp_demo_results), 
              info = "Demo results should include 'ultimate' claims.")
  expect_true("standard_error" %in% names(mackcp_demo_results), 
              info = "Demo results should include 'standard_error'.")
  
  # Additional specific checks can be placed here based on known outcomes
})

# Edge case tests for invalid inputs
test_that("MackCP handles NA values appropriately", {
  # Test with a dataset containing NA values
  data_with_na <- RAA
  data_with_na[1, 1] <- NA  # Introduce NA values
  expect_warning(mack_estimator(data_with_na),
                 "NA values found in input data", 
                 info = "MackCP should provide warnings for NA values.")
})

test_that("MackCP handles non-numeric data gracefully", {
  # Test with a dataset containing non-numeric values
  data_with_non_numeric <- RAA
  data_with_non_numeric[1, 1] <- "string"  # Introduce non-numeric data
  expect_error(mack_estimator(data_with_non_numeric),
               "Input triangle must be numeric",
               info = "Expected error message for non-numeric input.")
})
