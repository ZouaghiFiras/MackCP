library(testthat)
library(MackCP)
library(ChainLadder)

# Load the RAA dataset
data("RAA", package = "ChainLadder")

# Test cases for MackCP demo functionality
test_that("MackCP demo runs without errors", {
  expect_silent(demo_MackCP(), 
                info = "Demo should run without errors.")
})

test_that("MackCP results structure is correct", {
  # Apply MackCP's estimator
  mackcp_results <- mack_estimator(RAA)
  
  # Validate structure
  expect_true("ultimate" %in% names(mackcp_results), 
              info = "Results should include 'ultimate' claims.")
  expect_true("standard_error" %in% names(mackcp_results), 
              info = "Results should include 'standard_error'.")
  
  # Check data types
  expect_is(mackcp_results$ultimate, "numeric", 
            info = "'ultimate' claims should be numeric.")
  expect_is(mackcp_results$standard_error, "numeric", 
            info = "'standard_error' should be numeric.")
})

test_that("Comparison analysis with ChainLadder is consistent", {
  # Calculate results using MackCP package
  mackcp_results <- mack_estimator(RAA)
  mackcp_ultimate <- mackcp_results$ultimate
  mackcp_se <- mackcp_results$standard_error
  
  # Calculate results using ChainLadder package
  chainladder_results <- ChainLadder::MackChainLadder(RAA)
  chainladder_ultimate <- chainladder_results$FullTriangle[, ncol(chainladder_results$FullTriangle)]
  chainladder_se <- chainladder_results$Mack.S.E
  
  # Validate ultimate claims and standard errors
  expect_equal(mackcp_ultimate, chainladder_ultimate, tolerance = 0.01,
               info = "Ultimate claims should match within a tolerance of 0.01")
  
  expect_equal(mackcp_se, chainladder_se, tolerance = 0.01,
               info = "Standard errors should match within a tolerance of 0.01")
})

test_that("Demo handles edge cases", {
  # Test with empty dataset
  empty_data <- data.frame()  # Create an empty dataframe
  expect_error(mack_estimator(empty_data), 
               info = "MackCP should handle empty datasets gracefully.")
  
  # Test with a dataset containing missing values
  data_with_na <- RAA
  data_with_na[1, 1] <- NA  # Introduce NA values
  expect_warning(mack_estimator(data_with_na), 
                 info = "MackCP should provide warnings for NA values.")
})

test_that("Printed output is as expected", {
  # Capture printed output
  output <- capture.output(demo_MackCP())
  expect_true(grepl("MackCP's Estimator Results", output, fixed = TRUE), 
              info = "Output should mention 'MackCP's Estimator Results'.")
  expect_true(grepl("ChainLadder's Mack Estimator Results", output, fixed = TRUE), 
              info = "Output should mention 'ChainLadder's Mack Estimator Results'.")
})

# To run the tests, uncomment the following line:
# test_file("path_to_your_test_file.R")
