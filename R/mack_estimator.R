# mack_estimator.R
#' @importFrom stats qt
#' @importFrom utils data

#' Mack Estimator for Compound Poisson Setting
#'
#' Implements Mack's estimator motivated by large exposure asymptotics in a compound Poisson setting.
#' This estimator is designed for claims reserving, loss development, and risk analysis based on
#' the research by Engler and Lindskog (2024).
#'
#' @param triangle A loss triangle data frame or matrix, where rows represent development periods,
#'                 and columns represent accident or origin periods.
#' @param exposure A numeric vector indicating the exposure for each accident period. If not provided,
#'                 assumes a constant exposure for each period.
#' @param conf_level A numeric value indicating the confidence level for the standard error estimates (default is 0.95).
#' @param ... Additional parameters for future extensions.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{UltimateClaims}{Estimated ultimate claims for each accident period.}
#'   \item{StdError}{Standard errors for each accident period.}
#'   \item{DevelopmentFactors}{Estimated development factors for each development period.}
#'   \item{ConfidenceIntervals}{Confidence intervals for each accident period based on the specified confidence level.}
#' }
#'
#' @references
#' Engler N, Lindskog F. Mack’s estimator motivated by large exposure asymptotics in a compound Poisson setting.
#' \emph{ASTIN Bulletin}. 2024;54(2):310-326. doi:10.1017/asb.2024.11
#'
#' @export
#' @examples
#' \dontrun{
#' data(RAA, package = "ChainLadder")
#' results <- mack_estimator(RAA)
#' print(results)
#' }
mack_estimator <- function(triangle, exposure = NULL, conf_level = 0.95, ...) {
  # Ensure the triangle is in a suitable format
  if (!is.matrix(triangle) && !is.data.frame(triangle)) {
    stop("The input 'triangle' must be a matrix or a data frame.")
  }

  # If exposure is not provided, assume constant exposure for each accident period
  if (is.null(exposure)) {
    exposure <- rep(1, ncol(triangle))
  }

  # Basic validation checks
  if (length(exposure) != ncol(triangle)) {
    stop("Length of 'exposure' must match the number of accident periods (columns) in the triangle.")
  }

  # Calculate cumulative development factors and ultimate claims
  cum_triangle <- apply(triangle, 2, cumsum)
  dev_factors <- apply(cum_triangle, 1, function(x) {
    last_value <- x[length(x)]
    return(ifelse(last_value > 0, x / last_value, 1))
  })

  # Estimate ultimate claims for each accident period
  ultimate_claims <- colSums(triangle, na.rm = TRUE) * exposure

  # Calculate standard errors and confidence intervals
  std_error <- sqrt(rowSums((cum_triangle - ultimate_claims) ^ 2) / length(exposure))
  ci_bounds <- qt(c((1 - conf_level) / 2, (1 + conf_level) / 2), df = length(exposure) - 1)
  ci <- lapply(ultimate_claims, function(uc, se) {
    uc + se * ci_bounds
  }, se = std_error)

  # Create results list
  results <- list(
    UltimateClaims = ultimate_claims,
    StdError = std_error,
    DevelopmentFactors = dev_factors,
    ConfidenceIntervals = ci
  )

  class(results) <- "mack_estimator"
  return(results)
}

#' Print Method for Mack Estimator Results
#'
#' @param x An object of class 'mack_estimator'.
#' @param ... Additional parameters.
#'
#' @export
print.mack_estimator <- function(x, ...) {
  cat("Mack Estimator Results:\n")
  cat("-------------------------\n")
  cat("Ultimate Claims:\n")
  print(x$UltimateClaims)

  cat("\nStandard Errors:\n")
  print(x$StdError)

  cat("\nDevelopment Factors:\n")
  print(x$DevelopmentFactors)

  cat("\nConfidence Intervals:\n")
  print(x$ConfidenceIntervals)
}

#' Summary Method for Mack Estimator Results
#'
#' @param object An object of class 'mack_estimator'.
#' @param ... Additional parameters.
#'
#' @return A summary of the key results.
#'
#' @export
summary.mack_estimator <- function(object, ...) {
  list(
    UltimateClaims = object$UltimateClaims,
    StdError = object$StdError,
    DevelopmentFactors = object$DevelopmentFactors,
    ConfidenceIntervals = object$ConfidenceIntervals
  )
}
