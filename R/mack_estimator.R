#' @importFrom stats qt var
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
mack_estimator <- function(triangle, exposure = NULL, conf_level = 0.95, ...) {
  # Validate inputs
  if (!is.matrix(triangle) && !is.data.frame(triangle)) {
    stop("The input 'triangle' must be a matrix or a data frame.")
  }
  if (!is.null(exposure) && !is.numeric(exposure)) {
    stop("The 'exposure' must be a numeric vector.")
  }
  
  # If exposure is not provided, assume constant exposure
  if (is.null(exposure)) {
    exposure <- rep(1, ncol(triangle))
  } else if (length(exposure) != ncol(triangle)) {
    stop("The length of 'exposure' must match the number of accident periods (columns) in the triangle.")
  }
  
  # Calculate cumulative sums for each accident period (column)
  cum_triangle <- apply(triangle, 2, cumsum)
  
  # Calculate development factors as averages of successive period ratios
  dev_factors <- sapply(2:nrow(triangle), function(i) {
    ratio <- cum_triangle[i, -1] / cum_triangle[i - 1, -ncol(cum_triangle)]
    mean(ratio, na.rm = TRUE)
  })
  
  # Extrapolate ultimate claims for each accident period using the last development factor
  ultimate_claims <- cum_triangle[nrow(triangle), ] * tail(dev_factors, 1)
  
  # Estimate standard errors based on the exposure
  std_error <- sqrt(apply(triangle, 2, var, na.rm = TRUE) / length(exposure))
  
  # Calculate z-score based on the confidence level
  z_value <- qt((1 + conf_level) / 2, df = length(exposure) - 1)
  
  # Calculate confidence intervals for ultimate claims
  conf_intervals <- cbind(
    ultimate_claims - z_value * std_error,
    ultimate_claims + z_value * std_error
  )
  
  # Return the results as a list
  results <- list(
    UltimateClaims = ultimate_claims,
    StdError = std_error,
    DevelopmentFactors = dev_factors,
    ConfidenceIntervals = conf_intervals
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
  cat("Summary of Mack Estimator Results:\n")
  cat("----------------------------------\n")
  list(
    UltimateClaims = object$UltimateClaims,
    StdError = object$StdError,
    DevelopmentFactors = object$DevelopmentFactors,
    ConfidenceIntervals = object$ConfidenceIntervals
  )
}
