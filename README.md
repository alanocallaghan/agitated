<!-- README.md is generated from README.Rmd. Please edit that file -->

agitated
========

<!-- badges: start -->

[![R build
status](https://github.com/Alanocallaghan/agitated/workflows/R-CMD-check/badge.svg)](https://github.com/Alanocallaghan/agitated/actions)
[![CRAN\_Release\_Badge](http://www.r-pkg.org/badges/version-ago/agitated)](https://CRAN.R-project.org/package=agitated)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/agitated)](https://CRAN.R-project.org/package=agitated)
<!-- badges: end -->

An UpSetR clone to soothe my agitated heart

Installation
------------

You can install the released version of agitated from github with

    devtools::install_github("Alanocallaghan/agitated")

Example
-------

This is a basic example which shows you how to solve a common problem:

    library("here")
    #> here() starts at /home/alan/Documents/github/agitated
    library("devtools")
    #> Loading required package: usethis
    load_all(here())
    #> Loading agitated
    #> 
    #> Attaching package: 'testthat'
    #> The following object is masked from 'package:devtools':
    #> 
    #>     test_file
    library("agitated")
    ## exclusive intersections
    agitated(example_data())
    #> Warning: `expand_scale()` is deprecated; use `expansion()` instead.

    #> Warning: `expand_scale()` is deprecated; use `expansion()` instead.

<img src="man/figures/README-example-1.png" width="100%" />

    ## exclusive intersections, higher limit on number of sets (default is 20)
    agitated(example_data(), exclusive = FALSE, nsets = 30)
    #> Warning: `expand_scale()` is deprecated; use `expansion()` instead.

    #> Warning: `expand_scale()` is deprecated; use `expansion()` instead.

<img src="man/figures/README-example2-1.png" width="100%" />
