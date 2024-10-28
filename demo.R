#' Demo for the MackCP Package
#'
#' This demo showcases the capabilities of the MackCP package using the RAA dataset.
#' It compares the estimates of Mack's estimator from the MackCP package with those
#' from the chainladder package, providing a visual representation of the results.
#'
#' @examples
#' demo()  # Run the demo

demo <- function() {
  # Load necessary libraries
  if (!requireNamespace("MackCP", quietly = TRUE)) {
    stop("Please install the MackCP package first.")
  }

  if (!requireNamespace("chainladder", quietly = TRUE)) {
    stop("Please install the chainladder package first.")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install the ggplot2 package first.")
  }

  # Load the demo script
  source(system.file("demo/demo_script.R", package = "MackCP"))
}

# Run the demo when this script is executed directly
if (interactive()) {
  demo()
}
