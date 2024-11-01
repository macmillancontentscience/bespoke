
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bespoke

# bespoke <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/bespoke)](https://CRAN.R-project.org/package=bespoke)
[![Codecov test
coverage](https://codecov.io/gh/macmillancontentscience/bespoke/branch/main/graph/badge.svg)](https://app.codecov.io/gh/macmillancontentscience/bespoke?branch=main)
[![R-CMD-check](https://github.com/macmillancontentscience/bespoke/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/macmillancontentscience/bespoke/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Sometimes it makes sense to define a simple baseline model
programmatically, using simple rules defined by subject matter experts.
However, it can be difficult to compare such models to “real” models
developed in the [{tidymodels}](https://www.tidymodels.org/) framework.
{bespoke} allows you to wrap such a hand-crafted “model” into a format
that fits within the {tidymodels} framework.

## Installation

<div class="pkgdown-release">

Install the released version of bespoke from
[CRAN](https://cran.r-project.org/):

``` r
install.packages("bespoke")
```

</div>

<div class="pkgdown-devel">

Install the development version of bespoke from
[GitHub](https://github.com/):

``` r
# install.packages("pak")
pak::pak("macmillancontentscience/bespoke")
```

</div>

## Code of Conduct

Please note that the bespoke project is released with a [Contributor
Code of
Conduct](https://macmillancontentscience.github.io/bespoke/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
