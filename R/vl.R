#' vl
#'
#' Holds functions for making objects based on Vega-Lite spec. 
#' For example `vl$X(field = 'crop', type = 'nominal')` could 
#' be used to build a list with an X axis encoding. See online Vega-Lite documentation
#' for information about parameters.
#' 
#' @export
vl <- structure(new.env(parent = emptyenv()), class = c("vl", "environment"))

#' @export
print.vl <- function(x, ...) {
  print("Functions for making Vega-Lite components, e.g. vl$AggregateTransform")
}
