# Load the testthat package
library(testthat)

# Load the package being tested
library(MackCP)

# Run all tests in the 'tests/testthat' directory
test_dir("tests/testthat", reporter = "summary")
