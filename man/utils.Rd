\name{utils}
\alias{calculate_development_factors}
\alias{calculate_standard_errors}
\title{Utility Functions for MackCP Package}
\description{
  A set of utility functions for supporting the main estimation process in the MackCP package. These functions include calculation of development factors and standard errors based on provided input data.
}
\details{
  These utility functions are designed to aid the implementation of Mack's estimator within the MackCP package. The functions are meant to be internal helpers for the main \code{\link[MackCP]{mack_estimator}} function.
}
\section{Functions}{
  \itemize{
    \item \code{calculate_development_factors(triangle)}: Computes the development factors for each development period based on the input development triangle.

    \item \code{calculate_standard_errors(ultimate_claims, exposure)}: Computes the standard errors for the estimated ultimate claims based on the provided exposure values.
  }
}
\arguments{
  \item{triangle}{A development triangle in matrix format. Each row represents an accident year, and each column represents a development period.}
  \item{ultimate_claims}{A numeric vector representing the estimated ultimate claims for each accident year.}
  \item{exposure}{A numeric vector representing the exposure for each accident year. The length of the vector should match the length of \code{ultimate_claims}.}
}
\value{
  \code{calculate_development_factors} returns a numeric vector containing the estimated development factors for each development period.

  \code{calculate_standard_errors} returns a numeric vector containing the estimated standard errors for the ultimate claims of each accident year.
}
\author{
  Firas Zouaghi
}
\seealso{
  \code{\link[MackCP]{mack_estimator}}, \code{\link[ChainLadder]{MackChainLadder}}
}
\examples{
# Example of calculating development factors
data("RAA", package = "ChainLadder")
dev_factors <- calculate_development_factors(RAA)
print(dev_factors)

# Example of calculating standard errors
ultimate_claims <- c(1200, 1500, 1800, 2100, 2500)
exposure <- c(100, 120, 150, 180, 210)
std_errors <- calculate_standard_errors(ultimate_claims, exposure)
print(std_errors)
}
