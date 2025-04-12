# facomplex

<!-- badges: start -->

<!-- badges: end -->

The goal of facomplex is to estimate the simplicity of a factorial solution of multidimensional scales.

## Installation

You can install the development version of facomplex from [GitHub](https://github.com/) with:

``` r
# install.packages("facomplex")
facomplex::facomplex("cmerinos/facomplex")
```

## Example

This is a basic example which shows you how to solve a common problem:
# Example data
ex1_data <- data.frame(
F1 = c(0.536, 0.708, 0.600, 0.673, 0.767, 0.481, -0.177, 0.209, -0.097, -0.115, 0.047, 0.024),
F2 = c(-0.110, 0.026, 0.076, 0.011, -0.160, 0.106, 0.668, 0.438, 0.809, 0.167, 0.128, 0.041),
F3 = c(-0.100, 0.036, 0.086, 0.021, -0.150, 0.116, 0.678, 0.448, 0.819, 0.577, 0.738, 0.751))

BSI(ex1_data)

FSI(data = ex1_data, 
items_target = list(F1 = c(1, 2, 3, 4, 5, 6),
F2 = c(7, 8, 9),
F3 = c(10, 11, 12)))

Chofman(ex1_data)

``` r
library(facomplex)
## basic example code
```
