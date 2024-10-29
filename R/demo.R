#' MackCP Demo Script
#'
#' This script demonstrates the key capabilities of the MackCP package using the RAA dataset.
#' It applies Mack's estimator motivated by large exposure asymptotics in a compound Poisson setting,
#' compares the results with the ChainLadder package's Mack estimator, and provides a detailed comparison.
#'
#' @examples
#' \dontrun{
#' demo_MackCP()
#' }
#'
#' @export
demo_MackCP <- function() {
  message("----------------------------------------------------")
  message("    Welcome to the MackCP Package Demonstration    ")
  message("----------------------------------------------------")
  
  # Load the RAA dataset from the ChainLadder package
  message("Loading the RAA dataset...")
  if (!requireNamespace("ChainLadder", quietly = TRUE)) {
    stop("The 'ChainLadder' package is not installed. Please install it to proceed.")
  }
  
  data("RAA", package = "ChainLadder")
  if (!exists("RAA")) {
    stop("The RAA dataset could not be loaded. Please ensure that the ChainLadder package is installed.")
  }
  
  # Display a snapshot of the dataset
  message("Here is a snapshot of the RAA dataset:")
  print(head(RAA))
  
  # Apply MackCP's estimator on the dataset
  message("\nApplying MackCP's estimator...")
  MackCP_results <- mack_estimator(RAA)
  
  # Display the results from MackCP
  message("\nMackCP's Estimator Results:")
  print(summary(MackCP_results))
  
  # Apply ChainLadder's Mack estimator on the dataset for comparison
  message("\nApplying ChainLadder's Mack estimator for comparison...")
  ChainLadder_results <- ChainLadder::MackChainLadder(RAA)
  
  # Display the results from ChainLadder
  message("\nChainLadder's Mack Estimator Results:")
  print(summary(ChainLadder_results))
  
  # Compare results using the comparison function from MackCP
  message("\nComparing results between MackCP and ChainLadder...")
  comparison <- compare_estimators(
    data = RAA,
    MackCP_method = mack_estimator,
    ChainLadder_method = ChainLadder::MackChainLadder
  )
  
  # Display the comparison metrics
  message("\nComparison of Ultimate Claims and Standard Errors:")
  print(comparison$ComparisonMetrics)
  
  message("\nThank you for exploring the MackCP package demonstration!")
}

# To run the demo, uncomment the following line:
# demo_MackCP()
