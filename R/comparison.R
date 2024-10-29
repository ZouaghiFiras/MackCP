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
compare_estimators <- function(data, MackCP_method, ChainLadder_method, ...) {
  # Ensure that the data is in the required format
  if (is.null(data) || !inherits(data, "triangle")) {
    stop("The input dataset must be a claims triangle in the appropriate format.")
  }
  
  # Apply MackCP's estimator on the dataset
  message("Applying MackCP's implementation of the Mack Estimator...")
  mackcp_results <- MackCP_method(data, ...)
  
  # Apply ChainLadder's Mack Estimator on the dataset
  message("Applying ChainLadder's implementation of the Mack Estimator...")
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
  message("Comparison of MackCP and ChainLadder Results:")
  message("Ultimate Claims (MackCP):", paste(mackcp_summary$Ultimate, collapse = ", "), "\n")
  message("Ultimate Claims (ChainLadder):", paste(chainladder_summary$Ultimate, collapse = ", "), "\n")
  message("Standard Errors (MackCP):", paste(mackcp_summary$StdError, collapse = ", "), "\n")
  message("Standard Errors (ChainLadder):", paste(chainladder_summary$StdError, collapse = ", "), "\n")
  
  # Return a list with detailed results and the comparison metrics
  return(list(
    MackCP_Results = mackcp_summary,
    ChainLadder_Results = chainladder_summary,
    ComparisonMetrics = comparison_metrics
  ))
}
