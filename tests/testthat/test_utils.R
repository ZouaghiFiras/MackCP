# Load necessary libraries
library(testthat)
library(MackCP)

# Define test cases for the utility functions

test_that("utility functions work as expected", {
  # Example utility function: calculate_mean
  calculate_mean <- function(x) {
    if (length(x) == 0) return(NA)
    return(mean(x, na.rm = TRUE))
  }

  # Test calculate_mean function
  expect_equal(calculate_mean(c(1, 2, 3, 4, 5)), 3,
               info = "calculate_mean should return the correct mean.")
  expect_equal(calculate_mean(c(NA, 2, 3)), 2.5,
               info = "calculate_mean should handle NA values correctly.")
  expect_equal(calculate_mean(numeric(0)), NA,
               info = "calculate_mean should return NA for empty input.")
})

test_that("other utility functions work as expected", {
  # Example utility function: validate_triangle
  validate_triangle <- function(data) {
    if (!is.data.frame(data) || ncol(data) < 3) {
      stop("Data must be a valid claims triangle.")
    }
    return(TRUE)
  }

  # Test validate_triangle function
  expect_true(validate_triangle(data.frame(Development = c(1, 2), Origin = c(1, 1), Claims = c(100, 200))),
              info = "validate_triangle should return TRUE for valid triangle data.")

  expect_error(validate_triangle(data.frame(Development = c(1, 2))),
               "Data must be a valid claims triangle.",
               info = "validate_triangle should throw an error for invalid triangle data.")

  expect_error(validate_triangle(NULL),
               "Data must be a valid claims triangle.",
               info = "validate_triangle should throw an error for NULL input.")
})

# Add more tests for other utility functions as needed
