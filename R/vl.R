#' vl
#'
#' Holds functions for making objects based on Vega-Lite spec
#' @export
vl <- structure(new.env(parent = emptyenv()), class = c("vl", "environment"))

#' @export
print.vl <- function(x, ...) {
  print("Functions for making Vega-Lite components, e.g. vl$AggregateTransform")
}
