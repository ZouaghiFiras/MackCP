# Load necessary libraries
library(testthat)
library(MackCP)
library(ChainLadder)

# Define test cases for demo functionality

test_that("Demo runs without errors", {
  # Test that the demo script executes without any errors
  expect_silent(demo("demo", package = "MackCP"))
})

test_that("Demo results are as expected", {
  # Load the RAA dataset from ChainLadder
  data("RAA", package = "ChainLadder")

  # Step 1: Run MackCP's demo to get demo results
  demo_results <- demo("demo", package = "MackCP", echo = FALSE)

  # Extract the results from MackCP's estimator
  mackcp_demo_results <- mack_estimator(RAA)

  # Check that demo results match the results from running mack_estimator directly
  expect_equal(mackcp_demo_results$ultimate, demo_results$mackcp_ultimate, tolerance = 0.01,
               info = "Demo results for ultimate claims should match direct MackCP results within a tolerance of 0.01")

  expect_equal(mackcp_demo_results$standard_error, demo_results$mackcp_se, tolerance = 0.01,
               info = "Demo results for standard errors should match direct MackCP results within a tolerance of 0.01")
})

test_that("Comparison analysis in demo is consistent with ChainLadder results", {
  # Load the RAA dataset
  data("RAA", package = "ChainLadder")

  # Calculate results using MackCP package
  mackcp_results <- mack_estimator(RAA)
  mackcp_ultimate <- mackcp_results$ultimate
  mackcp_se <- mackcp_results$standard_error

  # Calculate results using ChainLadder package
  chainladder_results <- MackChainLadder(RAA, est.sigma = "Mack")
  chainladder_ultimate <- chainladder_results$FullTriangle[, ncol(chainladder_results$FullTriangle)]
  chainladder_se <- chainladder_results$Mack.S.E

  # Validate that demo results align with both MackCP and ChainLadder results
  expect_equal(mackcp_ultimate, chainladder_ultimate, tolerance = 0.01,
               info = "Ultimate claims calculated by MackCP and ChainLadder should match within a tolerance of 0.01")

  expect_equal(mackcp_se, chainladder_se, tolerance = 0.01,
               info = "Standard errors calculated by MackCP and ChainLadder should match within a tolerance of 0.01")
})
