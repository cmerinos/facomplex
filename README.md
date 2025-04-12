# facomplex

**facomplex** is an R package that provides tools for assessing factor complexity in exploratory and confirmatory factor analysis (EFA/CFA) solutions.

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version of **facomplex** from GitHub using:

``` r
# Install devtools if not already installed
install.packages("devtools")

# Then install the package from GitHub
devtools::install_github("cmerinos/facomplex", build_vignettes = TRUE)
```

## Overview

The package includes several methods for evaluating factor complexity:

-   **Hofman coefficient** (Hofman, 1977)
-   **Revised Hofman coefficient**
-   **Factor Simplicity Index (FSI)** (Fleming, 2003)
-   **Bentler’s Simplicity Index** (Bentler, 1977)
-   Descriptive statistics (min, max, mean) of target and non-target loadings
-   Visualization tools for complexity structures

## Example

Here's a basic example using `facomplex`:

``` r
library(facomplex)

# Example factor loading matrix
ex1_fl <- data.frame(
  F1 = c(0.536, 0.708, 0.600, 0.673, 0.767, 0.481, -0.177, 0.209, -0.097, -0.115, 0.047, 0.024),
  F2 = c(-0.110, 0.026, 0.076, 0.011, -0.160, 0.106, 0.668, 0.438, 0.809, 0.167, 0.128, 0.041),
  F3 = c(-0.100, 0.036, 0.086, 0.021, -0.150, 0.116, 0.678, 0.448, 0.819, 0.577, 0.738, 0.751)
)

# Run a complexity analysis function (e.g., FSI)
FSIout <- FSI(ex1_fl,  
     items_target = list(F1 = c(1, 2, 3, 4, 5, 6),
                         F2 = c(7, 8, 9),
                         F3 = c(10, 11, 12)))

# Visualize the results
plot.simplicity(
   data = FSIout$FSI_i,
   item.col = "Items",
   value.col = "FSI_i")
```

## References

-   Bentler, P. M. (1977). Factor Simplicity Index and Transformations. *Psychometrika*, *42*(2), 277–295. <https://doi.org/10.1007/BF02294054>

-   Fleming, J. S. (1985). An index of fit for factor scales. Educational and Psychological Measurement, 45, 725-728. [https://doi.org/10.1177/0013164485454002](https://psycnet.apa.org/doi/10.1177/0013164485454002)

-   Fleming, J. S. (2003). Computing measures of simplicity of fit for loadings in factor-analytically derived scales. Behavior Research Methods, Instruments, & Computers, 35, 520–524. <https://doi.org/10.3758/BF03195531>

-   Fleming, J., S. & Merino-Soto, C. (2005). Medidas de simplicidad y de ajuste factorial: un enfoque para la evaluación de escalas construidas factorialmente. Revista de Psicología, 23(2), 249-266.

-   Hofman, R. J. (1977). *Simplicity and complexity in factor analysis*. Multivariate Behavioral Research, 12(2), 149–165. <https://doi.org/10.18800/psico.200502.002>

-   Kaiser, H. F. (1974). An Index of Factorial Simplicity. *Psychometrika*, *39*(1), 31–36. [https://doi.org/](https://doi.org/10.1007/BF02294054){.uri}[10.1007/BF02291575](https://doi.org/10.1007/BF02291575)

## License

This package is licensed under the GPL-3.
