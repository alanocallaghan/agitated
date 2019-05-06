
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
#> here() starts at /home/alan/Documents/github/agitated
load_all(here())
#> Loading agitated
#> 
#> Attaching package: 'testthat'
#> The following objects are masked from 'package:devtools':
#> 
#>     setup, test_file
library("agitated")
## basic example code
agitated(example_data())
```

<img src="man/figures/README-example-1.png" width="100%" />
