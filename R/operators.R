#' Concatenation operators
#' 
#' Use `+` for layering, `|` for horizontal concatenation, `&` for vertical concatenation.
#' @param e1 vegalite spec
#' @param e2 vegalite spec
#' @return A new vegalite spec
#' @seealso [vl_layer()], [vl_hconcat()], [vl_vconcat()]
#' @name vegabrite_operators
NULL

#' @rdname vegabrite_operators
#' @export
#'
"|.vegaspec_vega_lite" <- function(e1, e2) {
  vl_hconcat(e1, e2)
}

#' @rdname vegabrite_operators
#' @export
#'
"+.vegaspec_vega_lite" <- function(e1, e2) {
  vl_layer(e1, e2)
}

#' @rdname vegabrite_operators
#' @export
#'
"&.vegaspec_vega_lite" <- function(e1, e2) {
  vl_vconcat(e1, e2)
}

