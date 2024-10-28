
# MackCP

## Comprehensive Implementation of Mack's Estimator in a Compound Poisson Setting

### Description

**MackCP** is an R package that implements Mack's estimator motivated by large exposure asymptotics in a compound Poisson setting. This package provides a suite of actuarial tools for claims reserving, loss development, and risk analysis. It aims to facilitate the analysis of insurance claims data and enhance the understanding of risk management practices in actuarial science.

### Installation

You can install the **MackCP** package from GitHub using the `devtools` package. First, ensure you have the `devtools` package installed:

```R
install.packages("devtools")
```

Then, install **MackCP**:

```R
devtools::install_github("yourusername/MackCP")
```

### Usage

Here are some basic examples of how to use the **MackCP** package.

#### Example: Using Mack's Estimator

```R
# Load the MackCP package
library(MackCP)

# Load your dataset, e.g., RAA dataset
data("RAA")

# Run the Mack estimator
result <- mack_estimator(data = RAA, exposure = "exposure_column")

# View the results
print(result)
```

#### Example: Running a Demo

```R
# Run the demo script
demo()
```

#### Example: Comparing with ChainLadder Package

```R
# Compare results with ChainLadder's mack estimator
comparison_result <- comparison(data = RAA)

# View comparison results
print(comparison_result)
```

### Vignettes

For more detailed examples and usage, check the vignettes provided in the package:

```R
browseVignettes("MackCP")
```

### Citation

If you use the **MackCP** package in your research or projects, please cite the following article:

Engler N, Lindskog F. Mack’s estimator motivated by large exposure asymptotics in a compound Poisson setting. *ASTIN Bulletin*. 2024;54(2):310-326. doi:10.1017/asb.2024.11

### Contributing

Contributions to **MackCP** are welcome! If you have suggestions for improvements or encounter any issues, please open an issue or submit a pull request on the GitHub repository.

### License

This package is licensed under the [GPL-3 License](https://www.gnu.org/licenses/gpl-3.0.html).

### Contact

For questions or feedback, please contact:

- **Firas Zouaghi** - [firas.zouaghi@enicar.ucar.tn](mailto:firas.zouaghi@enicar.ucar.tn)
