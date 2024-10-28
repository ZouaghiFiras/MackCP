# demo.R

#' MackCP Demo Script
#'
#' This script demonstrates the key capabilities of the MackCP package using the RAA dataset.
#' It applies Mack's estimator motivated by large exposure asymptotics in a compound Poisson setting,
#' compares the results with the ChainLadder package's Mack estimator, and provides a detailed comparison.
#'
#' @importFrom stats qt
#' @importFrom utils data
#' @import MackCP
#' @import ChainLadder
#' @examples
#' \dontrun{
#' demo_MackCP()
#' }

demo_MackCP <- function() {
  cat("----------------------------------------------------\n")
  cat("    Welcome to the MackCP Package Demonstration    \n")
  cat("----------------------------------------------------\n")

  # Load the RAA dataset from the ChainLadder package
  cat("Loading the RAA dataset...\n")
  data("RAA", package = "ChainLadder")

  if (!exists("RAA")) {
    stop("The RAA dataset could not be loaded. Please ensure that the ChainLadder package is installed.")
  }

  # Display the first few rows of the dataset
  cat("Here is a snapshot of the RAA dataset:\n")
  print(head(RAA))

  # Apply MackCP's estimator on the dataset
  cat("\nApplying MackCP's estimator...\n")
  MackCP_results <- mack_estimator(RAA)

  # Display the results from MackCP
  cat("\nMackCP's Estimator Results:\n")
  print(summary(MackCP_results))

  # Apply ChainLadder's Mack estimator on the dataset for comparison
  cat("\nApplying ChainLadder's Mack estimator for comparison...\n")
  ChainLadder_results <- ChainLadder::MackChainLadder(RAA)

  # Display the results from ChainLadder
  cat("\nChainLadder's Mack Estimator Results:\n")
  print(summary(ChainLadder_results))

  # Compare results using the comparison function from MackCP
  cat("\nComparing results between MackCP and ChainLadder...\n")
  comparison <- compare_estimators(
    data = RAA,
    MackCP_method = mack_estimator,
    ChainLadder_method = ChainLadder::MackChainLadder
  )

  # Display the comparison metrics
  cat("\nComparison of Ultimate Claims and Standard Errors:\n")
  print(comparison$ComparisonMetrics)

  cat("\nThank you for exploring the MackCP package demonstration!\n")
}

# To run the demo, uncomment the following line:
# demo_MackCP()
