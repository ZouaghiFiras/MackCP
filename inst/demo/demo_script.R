# Load necessary libraries
library(MackCP)
library(chainladder)
library(ggplot2)
library(gridExtra)

# Load the RAA dataset
data("RAA", package = "chainladder")

# Function to display results in a more user-friendly manner
display_results <- function(mack_cp_results, chainladder_results) {
  cat("\n-------------------------------------------------\n")
  cat("                 MackCP Results                   \n")
  cat("-------------------------------------------------\n")
  print(mack_cp_results)
  
  cat("\n-------------------------------------------------\n")
  cat("               ChainLadder Results                \n")
  cat("-------------------------------------------------\n")
  print(chainladder_results)
}

# Prepare and plot comparison of estimates
plot_comparison <- function(mack_cp_results, chainladder_results) {
  comparison_data <- data.frame(
    Development_Year = rownames(mack_cp_results$estimates),
    MackCP_Estimate = mack_cp_results$estimates,
    Chainladder_Estimate = chainladder_results$estimates
  )
  
  comparison_plot <- ggplot(comparison_data, aes(x = Development_Year)) +
    geom_line(aes(y = MackCP_Estimate, color = "MackCP"), size = 1) +
    geom_line(aes(y = Chainladder_Estimate, color = "Chainladder"), size = 1) +
    labs(title = "Comparison of Mack's Estimators",
         x = "Development Year",
         y = "Estimates",
         color = "Method") +
    theme_minimal() +
    theme(legend.position = "top")
  
  # Save the plot as a PNG file
  ggsave("comparison_plot.png", plot = comparison_plot)
  
  return(comparison_plot)
}

# Run the demo
cat("----------------------------------------------------\n")
cat("    Welcome to the MackCP Package Demonstration    \n")
cat("----------------------------------------------------\n")

# Display the structure of the RAA dataset
cat("Loading the RAA dataset...\n")
cat("Structure of RAA dataset:\n")
str(RAA)

# Use Mack's estimator from MackCP
cat("\nApplying Mack's estimator from MackCP...\n")
mack_cp_results <- mack_estimator(RAA)

# Use Mack's estimator from chainladder package
cat("\nApplying Mack's estimator from ChainLadder...\n")
mack_chainladder_results <- MackChainladder(RAA)

# Display results
display_results(mack_cp_results, mack_chainladder_results)

# Plot the comparison of estimates
comparison_plot <- plot_comparison(mack_cp_results, mack_chainladder_results)

# Print the plot
print(comparison_plot)

# Summary of the comparison
comparison_summary <- data.frame(
  Development_Year = rownames(mack_cp_results$estimates),
  MackCP_Estimate = mack_cp_results$estimates,
  Chainladder_Estimate = chainladder_results$estimates
)

cat("\n-------------------------------------------------\n")
cat("Summary of the Comparison between MackCP and ChainLadder:\n")
print(summary(comparison_summary))
