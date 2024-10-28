# Load necessary libraries
library(MackCP)
library(chainladder)
library(ggplot2)

# Load the RAA dataset
data("RAA", package = "chainladder")

# Display the structure of the RAA dataset
print("Structure of RAA dataset:")
str(RAA)

# Use Mack's estimator from MackCP
mack_cp_results <- mack_estimator(RAA)

# Print MackCP results
print("MackCP Results:")
print(mack_cp_results)

# Use Mack's estimator from chainladder package
mack_chainladder_results <- MackChainladder(RAA)

# Print chainladder results
print("Chainladder Results:")
print(mack_chainladder_results)

# Prepare data for comparison
comparison_data <- data.frame(
  Development_Year = rownames(mack_cp_results$estimates),
  MackCP_Estimate = mack_cp_results$estimates,
  Chainladder_Estimate = mack_chainladder_results$estimates
)

# Plot the comparison of estimates
comparison_plot <- ggplot(comparison_data, aes(x = Development_Year)) +
  geom_line(aes(y = MackCP_Estimate, color = "MackCP"), size = 1) +
  geom_line(aes(y = Chainladder_Estimate, color = "Chainladder"), size = 1) +
  labs(title = "Comparison of Mack's Estimators",
       x = "Development Year",
       y = "Estimates",
       color = "Method") +
  theme_minimal()

# Print the plot
print(comparison_plot)

# Save the plot as a PNG file
ggsave("inst/demo/comparison_plot.png", plot = comparison_plot)

# Summary of the comparison
print("Summary of the comparison between MackCP and Chainladder estimates:")
summary(comparison_data)
