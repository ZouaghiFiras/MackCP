\name{run_demo}
\alias{run_demo}
\title{Run Demonstration of Mack's Estimator in a Compound Poisson Setting}
\description{
  Runs a demonstration showcasing the implementation of Mack's estimator in the \code{MackCP} package using the \code{RAA} dataset. This demo compares the results with the \code{ChainLadder} package and performs a comprehensive analysis of key metrics.
}
\usage{
run_demo()
}
\details{
  The \code{run_demo} function is designed to give users a hands-on demonstration of how Mack's estimator is implemented in the \code{MackCP} package. It leverages the \code{RAA} dataset (available from the \code{ChainLadder} package) to perform claims reserving, calculate ultimate claims, development factors, and standard errors, and provide a side-by-side comparison with the \code{ChainLadder} package's implementation.

  This function helps users understand the functionality of \code{mack_estimator}, and it highlights the differences and similarities in results with the \code{MackChainLadder} function from the \code{ChainLadder} package.
}
\value{
  A data frame that presents a comprehensive comparison of key results obtained using Mack's estimator from the \code{MackCP} package and the \code{ChainLadder} package. This includes ultimate claims, development factors, and standard errors.
}
\examples{
\dontrun{
  # Run the demonstration
  run_demo()
}
}
\seealso{
  \code{\link[MackCP]{mack_estimator}}, \code{\link[MackCP]{compare_mack_estimators}}, \code{\link[ChainLadder]{MackChainLadder}}
}
\author{
  Firas Zouaghi
}
