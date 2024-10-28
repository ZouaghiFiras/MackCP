# Load the testthat package
library(testthat)

# Load the package being tested
library(MackCP)

# Source individual test files
test_dir("tests/testthat", reporter = "summary")
