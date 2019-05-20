<!-- README.md is generated from README.Rmd. Please edit that file -->
![lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

vlbuildr
========

The goal of vlbuildr is to provide an R api for building up vega-lite
specs. This package is in very early stages at the moment… very
incomplete!

Example
-------

This is an example showing current capabilities:

``` r
library(vlbuildr)
vl_chart() %>%
   vl_add_data(values = mtcars) %>%
   vl_mark_point() %>%
   vl_encode_x(field = "wt", type = "quantitative") %>%
   vl_encode_y(field = "mpg", type = "quantitative")
```
