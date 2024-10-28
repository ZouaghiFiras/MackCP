# utils.R

#' Calculate Cumulative Triangle
#'
#' Converts an incremental loss triangle into a cumulative loss triangle.
#'
#' @param triangle A matrix or data frame representing an incremental loss triangle, where rows represent development periods
#'                 and columns represent accident periods.
#' @return A matrix or data frame of the same dimension as `triangle`, with cumulative losses by row.
#' @export
#' @examples
#' \dontrun{
#' data(RAA, package = "ChainLadder")
#' cumulative_triangle <- calculate_cumulative_triangle(RAA)
#' print(cumulative_triangle)
#' }
calculate_cumulative_triangle <- function(triangle) {
  if (!is.matrix(triangle) && !is.data.frame(triangle)) {
    stop("The input 'triangle' must be a matrix or a data frame.")
  }

  apply(triangle, 2, cumsum)
}

#' Calculate Development Factors
#'
#' Calculates the development factors for each development period based on cumulative losses.
#'
#' @param cumulative_triangle A matrix or data frame representing the cumulative loss triangle, where rows are development periods
#'                            and columns are accident periods.
#' @return A numeric vector containing the development factors for each development period.
#' @export
#' @examples
#' \dontrun{
#' data(RAA, package = "ChainLadder")
#' cumulative_triangle <- calculate_cumulative_triangle(RAA)
#' dev_factors <- calculate_development_factors(cumulative_triangle)
#' print(dev_factors)
#' }
calculate_development_factors <- function(cumulative_triangle) {
  if (!is.matrix(cumulative_triangle) && !is.data.frame(cumulative_triangle)) {
    stop("The input 'cumulative_triangle' must be a matrix or a data frame.")
  }

  dev_factors <- numeric(nrow(cumulative_triangle) - 1)
  for (i in 1:(nrow(cumulative_triangle) - 1)) {
    dev_factors[i] <- mean(cumulative_triangle[i + 1, ] / cumulative_triangle[i, ], na.rm = TRUE)
  }

  return(dev_factors)
}

#' Calculate Standard Error
#'
#' Estimates the standard error for each accident period based on residuals.
#'
#' @param ultimate_claims A numeric vector of ultimate claims for each accident period.
#' @param cumulative_triangle A matrix or data frame representing the cumulative loss triangle.
#' @return A numeric vector containing the standard error for each accident period.
#' @export
#' @examples
#' \dontrun{
#' data(RAA, package = "ChainLadder")
#' cumulative_triangle <- calculate_cumulative_triangle(RAA)
#' ultimate_claims <- rowSums(cumulative_triangle, na.rm = TRUE)
#' std_error <- calculate_standard_error(ultimate_claims, cumulative_triangle)
#' print(std_error)
#' }
calculate_standard_error <- function(ultimate_claims, cumulative_triangle) {
  if (!is.numeric(ultimate_claims)) {
    stop("The input 'ultimate_claims' must be a numeric vector.")
  }
  if (!is.matrix(cumulative_triangle) && !is.data.frame(cumulative_triangle)) {
    stop("The input 'cumulative_triangle' must be a matrix or a data frame.")
  }

  # Compute residuals based on differences between actual and estimated ultimate claims
  residuals <- cumulative_triangle - ultimate_claims

  # Calculate standard error as the root mean squared error of residuals for each accident period
  std_error <- apply(residuals, 2, function(res) {
    sqrt(mean(res^2, na.rm = TRUE))
  })

  return(std_error)
}
