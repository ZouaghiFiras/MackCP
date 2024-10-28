# comparison.R

#' Compare MackCP Estimator with ChainLadder's Mack Estimator
#'
#' This function compares the results obtained from the Mack estimator implementation
#' in the MackCP package with those from the ChainLadder package. It uses the RAA dataset as an example.
#'
#' @param data A dataset (default is `RAA`) containing claims triangle data.
#' @param MackCP_method Function or method from the MackCP package used to calculate the estimator.
#' @param ChainLadder_method Function or method from the ChainLadder package used to calculate the estimator.
#' @param ... Additional parameters to pass to both methods if needed.
#' @return A list containing results from both MackCP and ChainLadder methods and a summary comparison.
#' @importFrom ChainLadder MackChainLadder
#' @examples
#' \dontrun{
#' compare_estimators(data = RAA, MackCP_method = mack_estimator, ChainLadder_method = ChainLadder::MackChainLadder)
#' }
#' @export
compare_estimators <- function(data = RAA, MackCP_method, ChainLadder_method, ...) {
  # Ensure that the data is in the required format
  if (is.null(data) || !inherits(data, "triangle")) {
    stop("The input dataset must be a claims triangle in the appropriate format.")
  }

  # Apply MackCP's estimator on the dataset
  cat("Applying MackCP's implementation of the Mack Estimator...\n")
  mackcp_results <- MackCP_method(data, ...)

  # Apply ChainLadder's Mack Estimator on the dataset
  cat("Applying ChainLadder's implementation of the Mack Estimator...\n")
  chainladder_results <- ChainLadder_method(data, ...)

  # Extract relevant results from both methods
  mackcp_summary <- summary(mackcp_results)
  chainladder_summary <- summary(chainladder_results)

  # Calculate comparison metrics (e.g., ultimate claims, standard errors, etc.)
  comparison_metrics <- list(
    UltimateClaimsDifference = mackcp_summary$Ultimate - chainladder_summary$Ultimate,
    StdErrorDifference = mackcp_summary$StdError - chainladder_summary$StdError
  )

  # Output the comparison results
  cat("Comparison of MackCP and ChainLadder Results:\n")
  cat("Ultimate Claims (MackCP):\n", mackcp_summary$Ultimate, "\n")
  cat("Ultimate Claims (ChainLadder):\n", chainladder_summary$Ultimate, "\n")
  cat("Standard Errors (MackCP):\n", mackcp_summary$StdError, "\n")
  cat("Standard Errors (ChainLadder):\n", chainladder_summary$StdError, "\n")

  # Return a list with detailed results and the comparison metrics
  return(list(
    MackCP_Results = mackcp_summary,
    ChainLadder_Results = chainladder_summary,
    ComparisonMetrics = comparison_metrics
  ))
}
