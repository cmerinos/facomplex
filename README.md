# facomplex

**facomplex** is an R package that provides tools for assessing factor complexity in exploratory and confirmatory factor analysis (EFA/CFA) solutions.

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version of **facomplex** from GitHub using:

``` r
# If you don't have 'pak' installed:
install.packages("pak")

# Then install the package
pak::pak("cmerinos/facomplex")
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

# Example dataset (included in the package)
data(FSI.out)

# Apply FSI function
FSI(FSI.out)

# Visualize
plot.simp1icity(FSI.out)
```

## References

-   Hofman, R. J. (1977). *Simplicity and complexity in factor analysis*. Multivariate Behavioral Research, 12(2), 149–165.
-   Fleming, M. (2003). *Factor simplicity index for item and factor clarity*.
-   Bentler, P. M. (1977). *Factor simplicity index*. Psychological Bulletin, 84(1), 115–117.

## License

This package is licensed under the GPL-3.
