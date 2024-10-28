# Load necessary libraries
library(testthat)
library(MackCP)
library(ChainLadder)

# Define test cases for demo functionality
test_that("Demo runs without errors", {
  # Test that the demo script executes without any errors
  expect_silent(demo("MackCP"),  # Ensure the correct demo function is called
                info = "Demo should run without errors.")
})

test_that("Demo results are as expected", {
  # Load the RAA dataset from ChainLadder
  data("RAA", package = "ChainLadder")
  
  # Step 1: Run MackCP's demo to get demo results
  mackcp_demo_results <- mack_estimator(RAA)
  
  # Check that the demo results have the expected structure
  expect_true("ultimate" %in% names(mackcp_demo_results), 
              info = "Results should include 'ultimate' claims.")
  expect_true("standard_error" %in% names(mackcp_demo_results), 
              info = "Results should include 'standard_error'.")
})

test_that("Comparison analysis in demo is consistent with ChainLadder results", {
  # Load the RAA dataset
  data("RAA", package = "ChainLadder")
  
  # Calculate results using MackCP package
  mackcp_results <- mack_estimator(RAA)
  mackcp_ultimate <- mackcp_results$ultimate
  mackcp_se <- mackcp_results$standard_error
  
  # Calculate results using ChainLadder package
  chainladder_results <- ChainLadder::MackChainLadder(RAA, est.sigma = "Mack")
  chainladder_ultimate <- chainladder_results$FullTriangle[, ncol(chainladder_results$FullTriangle)]
  chainladder_se <- chainladder_results$Mack.S.E
  
  # Validate that demo results align with both MackCP and ChainLadder results
  expect_equal(mackcp_ultimate, chainladder_ultimate, tolerance = 0.01,
               info = "Ultimate claims calculated by MackCP and ChainLadder should match within a tolerance of 0.01")
  
  expect_equal(mackcp_se, chainladder_se, tolerance = 0.01,
               info = "Standard errors calculated by MackCP and ChainLadder should match within a tolerance of 0.01")
})
