
<!-- README.md is generated from README.Rmd. Please edit that file -->
agitated
========

[![Travis-CI Build Status](https://travis-ci.org/Alanocallaghan/agitated.svg?branch=master)](https://travis-ci.org/Alanocallaghan/agitated) [![CRAN\_Release\_Badge](http://www.r-pkg.org/badges/version-ago/agitated)](https://CRAN.R-project.org/package=agitated) [![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/agitated)](https://CRAN.R-project.org/package=agitated)

An UpSetR clone to soothe my agitated heart

Installation
------------

You can install the released version of agitated from github with

``` r
devtools::install_github("Alanocallaghan/agitated")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library("here")
load_all(here())
#> Loading agitated
library("agitated")
## exclusive intersections
agitated(example_data())
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
## exclusive intersections, higher limit on number of sets (default is 20)
agitated(example_data(), exclusive = FALSE, nsets = 30)
```

<img src="man/figures/README-example2-1.png" width="100%" />
