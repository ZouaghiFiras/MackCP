\name{compare_mack_estimators}
\alias{compare_mack_estimators}
\title{Compare Mack's Estimator from MackCP with ChainLadder Package}
\description{
  Compares the results of Mack's estimator implemented in the \code{MackCP} package with those from the \code{ChainLadder} package. The function provides a side-by-side comparison of key metrics such as ultimate claims, development factors, and standard errors.
}
\usage{
compare_mack_estimators(triangle)
}
\arguments{
  \item{triangle}{A matrix or data frame representing an incremental loss triangle, where rows represent development periods and columns represent accident periods.}
}
\details{
  The \code{compare_mack_estimators} function performs a comparative analysis of Mack's estimator results obtained using the \code{mack_estimator} function from the \code{MackCP} package and the \code{MackChainLadder} function from the \code{ChainLadder} package. The comparison includes calculations for ultimate claims, development factors, and standard errors, and outputs these results in a tabular format for easy interpretation.
}
\value{
  A list containing:
  \item{MackCP_results}{A data frame with ultimate claims, development factors, and standard errors calculated using the \code{mack_estimator} function from the \code{MackCP} package.}
  \item{ChainLadder_results}{A data frame with ultimate claims, development factors, and standard errors calculated using the \code{MackChainLadder} function from the \code{ChainLadder} package.}
  \item{Comparison_table}{A data frame that presents a side-by-side comparison of the results from both packages.}
}
\examples{
\dontrun{
  # Load the RAA dataset from the ChainLadder package
  data(RAA, package = "ChainLadder")

  # Perform a comparison of Mack's estimators
  comparison_results <- compare_mack_estimators(RAA)

  # View the comparison results
  print(comparison_results$Comparison_table)
}
}
\seealso{
  \code{\link[MackCP]{mack_estimator}}, \code{\link[ChainLadder]{MackChainLadder}}
}
\author{
  Firas Zouaghi
}
