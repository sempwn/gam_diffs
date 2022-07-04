
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gamdiffs

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/gamdiffs)](https://CRAN.R-project.org/package=gamdiffs)
[![R-CMD-check](https://github.com/sempwn/gamdiffs/workflows/R-CMD-check/badge.svg)](https://github.com/sempwn/gamdiffs/actions)
<!-- badges: end -->

The goal of gamdiffs is to provide a set of convenience functions for
the estimation and associated standard errors / confidence intervals for
a range of Generalized Additive Models. The particular motivation is to
provide tools for estimating the difference in outcome under certain
counterfactual scenarios for models relevant to population public
health.

Current implemented confidence interval estimates are

-   delta method
-   posterior sampling
-   Efron bootstrap sampling

## Installation

You can install the released version of gamdiffs from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("gamdiffs")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sempwn/gamdiffs")
```

## Example

This basic example generates some data and fit a thin-plate spline to a
time variable (`x`) with outcome `y` which is Poisson-distributed. The
sum of the outcome between time 0 to 20 is then compared to the outcome
between time 21 to time 40 and the resulting expected difference with
associated confidence intervals is provided

``` r
library(gamdiffs)
library(mgcv)
#> Loading required package: nlme
#> This is mgcv 1.8-35. For overview type 'help("mgcv-package")'.
## basic usage
res <- gamdiffs:::create_random_data()
m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
baseline_data <- dplyr::tibble(x = 0:20)
counter_data <- dplyr::tibble(x = 21:40)
test_diffs <- calc_sum_counterfactual_gam(m, baseline_data,
  counter_data = counter_data,
  ci = 0.95
)
```

The resulting expected sum difference between the `baseline_data` and
`counter_data` are

``` r
print(test_diffs)
#> $m
#> [1] -18
#> 
#> $lc
#> [1] -38
#> 
#> $uc
#> [1] 1.4
```

The default setting is to estimate the confidence interval from the
delta method. The resulting confidence interval from sampling of the
posterior (with improper priors) can be calculated as

``` r
post_diffs <- calc_sum_counterfactual_gam(m, 
  baseline_data,
  use_post = TRUE,
  nrep = 1000,
  counter_data = counter_data,
  ci = 0.95
)
print(post_diffs)
#> $m
#> [1] -18
#> 
#> $lc
#> 2.5% 
#>  -39 
#> 
#> $uc
#> 97.5% 
#>   2.2
```

## Code of Conduct

Please note that the gamdiffs project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
