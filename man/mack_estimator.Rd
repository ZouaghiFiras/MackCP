\name{mack_estimator}
\alias{mack_estimator}
\title{Mack's Estimator in a Compound Poisson Setting}
\description{
  Computes Mack's estimator for claims reserving in a compound Poisson setting. This function leverages large exposure asymptotics and implements the methodology proposed by Engler and Lindskog (2024).
}
\usage{
mack_estimator(triangle, exposure, alpha = 1)
}
\arguments{
  \item{triangle}{A development triangle in matrix format. Each row represents an accident year, and each column represents a development period.}
  \item{exposure}{A numeric vector representing the exposure for each accident year. The length of the vector should be equal to the number of rows in the \code{triangle}.}
  \item{alpha}{A numeric parameter specifying the weighting of large exposure asymptotics. The default value is 1, which corresponds to standard Mack estimation. Lower values decrease the weight given to large exposures.}
}
\details{
  The \code{mack_estimator} function computes ultimate claims and estimates the standard errors of claims reserves using Mack's estimator, which is adapted to a compound Poisson setting with large exposure asymptotics. The implementation follows the approach proposed by Engler and Lindskog (2024) in their study published in the \emph{ASTIN Bulletin}.
}
\value{
  A list with the following elements:
  \item{ultimate_claims}{Estimated ultimate claims for each accident year.}
  \item{development_factors}{Estimated development factors for each development period.}
  \item{standard_errors}{Standard errors of the ultimate claims for each accident year.}
}
\references{
  Engler N, Lindskog F. Mack’s estimator motivated by large exposure asymptotics in a compound Poisson setting. \emph{ASTIN Bulletin}. 2024;54(2):310-326. doi:10.1017/asb.2024.11
}
\examples{
# Load a sample development triangle and exposure vector
data("RAA", package = "ChainLadder")
exposure <- c(100, 120, 150, 180, 210, 250, 300, 350, 400, 450)

# Run Mack's estimator using the MackCP package
results <- mack_estimator(RAA, exposure, alpha = 0.9)

# View the estimated ultimate claims and standard errors
print(results$ultimate_claims)
print(results$standard_errors)
}
\seealso{
  \code{\link[ChainLadder]{MackChainLadder}}, \code{\link[MackCP]{compare_mack_estimators}}, \code{\link[MackCP]{run_demo}}
}
\author{
  Firas Zouaghi
}
