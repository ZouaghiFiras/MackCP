# Load necessary libraries
library(testthat)

# Test the calculate_cumulative_triangle function
test_that("calculate_cumulative_triangle returns expected results", {
  # Create a sample incremental triangle
  triangle <- matrix(c(100, 200, NA, 300, 400, 500, 600, 700, 800), nrow = 3, byrow = TRUE)
  
  # Calculate cumulative triangle
  cumulative <- calculate_cumulative_triangle(triangle)
  
  # Expected cumulative triangle
  expected_cumulative <- matrix(c(100, 300, 300, 
                                  200, 600, 700, 
                                  300, 800, 1300), 
                                nrow = 3, byrow = TRUE)
  
  # Check the structure and values of the result
  expect_equal(cumulative, expected_cumulative, 
               info = "Cumulative triangle should match expected results.")
  expect_true(is.matrix(cumulative), 
              info = "Output should be a matrix.")
  expect_equal(dim(cumulative), dim(triangle), 
               info = "Output dimensions should match input dimensions.")
})

test_that("calculate_cumulative_triangle handles invalid input gracefully", {
  # Test with invalid data types
  expect_error(calculate_cumulative_triangle("invalid"), 
               "The input 'triangle' must be a matrix or a data frame.", 
               info = "The function should throw an error for invalid input types.")
  
  # Test with empty matrix
  empty_matrix <- matrix(numeric(0), nrow = 0, ncol = 0)
  expect_error(calculate_cumulative_triangle(empty_matrix), 
               "The input 'triangle' must have at least one row and one column.", 
               info = "The function should throw an error for empty input matrix.")
  
  # Test with zero rows
  expect_error(calculate_cumulative_triangle(matrix(nrow = 0, ncol = 1)),
               "The input 'triangle' must have at least one row and one column.", 
               info = "The function should throw an error for zero rows.")
  
  # Test with zero columns
  expect_error(calculate_cumulative_triangle(matrix(nrow = 1, ncol = 0)),
               "The input 'triangle' must have at least one row and one column.", 
               info = "The function should throw an error for zero columns.")
})

# Test the calculate_standard_error function
test_that("calculate_standard_error returns expected results", {
  # Create a sample cumulative triangle
  cumulative_triangle <- matrix(c(100, 300, 500, 
                                  200, 600, 800), 
                                nrow = 2, byrow = TRUE)
  
  # Create ultimate claims
  ultimate_claims <- c(300, 800, 1300) # Adjusted based on the cumulative triangle
  
  # Calculate standard error
  std_error <- calculate_standard_error(ultimate_claims, cumulative_triangle)
  
  # Expected standard error (adjust based on your understanding of the data)
  expected_std_error <- c(0, 0, 0)  # As the cumulative claims match the ultimate claims
  
  # Check the result
  expect_equal(std_error, expected_std_error, 
               info = "Standard error should match expected results.")
  expect_true(is.numeric(std_error), 
              info = "Output should be numeric.")
  expect_equal(length(std_error), ncol(cumulative_triangle), 
               info = "Output length should match number of columns in cumulative triangle.")
})

test_that("calculate_standard_error handles invalid input gracefully", {
  # Test with invalid ultimate claims
  expect_error(calculate_standard_error("invalid", matrix(1:6, nrow = 2)),
               "The input 'ultimate_claims' must be a numeric vector.",
               info = "The function should throw an error for invalid ultimate claims input.")
  
  # Test with mismatched dimensions
  expect_error(calculate_standard_error(c(1, 2), matrix(1:6, nrow = 3)),
               "The length of 'ultimate_claims' must match the number of columns in 'cumulative_triangle'.",
               info = "The function should throw an error for dimension mismatch.")
})
