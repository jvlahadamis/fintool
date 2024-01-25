
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fintool

<!-- badges: start -->
<!-- badges: end -->

The goal of fintool is to create a more efficient workflow for trading
and fintech functions.

## Installation

You can install the development version of fintool like so:

``` r
#install.packages("devtools")
devtools::install_github("jvlahadamis/fintool")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(fintool)

rollReg(mtcars, 5, "hp", "mpg")
```
