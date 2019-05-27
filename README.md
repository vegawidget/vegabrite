<!-- README.md is generated from README.Rmd. Please edit that file -->
![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

vlbuildr
========

The goal of vlbuildr is to provide an R api for building up vega-lite
specs. This package is in very early stages at the moment… API likely to
change!

Examples
--------

These are some examples showing current capabilities (note, might not
actually show up here on github due to current issues rendering to
markdown format, see pkgdown site for these and more examples.)

``` r
library(vlbuildr)
library(vegawidget)
vl_chart() %>%
   vl_add_data(values = mtcars) %>%
   vl_mark_point() %>%
   vl_encode_x(field = "wt", type = "quantitative") %>%
   vl_encode_y(field = "mpg", type = "quantitative") 
```

![](man/figures/README-example-1.png)

``` r
vl_chart() %>%
  vl_add_data(url = "https://vega.github.io/vega-editor/app/data/population.json") %>%
  vl_calculate(calculate = "datum.sex == 2 ? 'Female' : 'Male'", 
               as = "gender") %>%
  vl_filter("datum.year == 2000") %>%
  vl_encode_x(field = "age", 
              type = "ordinal") %>%
  vl_scale_x(rangeStep = 17) %>%
  vl_encode_y(field = "people", type = "quantitative") %>%
  vl_stack_y("normalize") %>%
  vl_aggregate_y("sum") %>%
  vl_axis_y(title = "population") %>%
  vl_encode_color(field = "gender", type = "nominal") %>%
  vl_mark_bar() 
```

![](man/figures/README-example2-1.png)
