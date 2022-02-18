#' Deprecated functions from vegabrite
#'
#' These functions have been deprecated in favor of a different
#' naming convention.
#' @param ... argument to pass to new function
#' @name vegabrite-deprecated
NULL

#' @rdname vegabrite-deprecated
#' @export
vl_facet_wrap <- function(...) {
  .Deprecated('vl_facet', package = 'vegabrite', "vl_facet_wrap has been replaced with vl_facet and will be removed in future package versions")
  vl_facet(...)
}

#' @rdname vegabrite-deprecated
#' @export
vl_encode_wrap <- function(...) {
  .Deprecated('vl_encode_facet', package = 'vegabrite', "vl_encode_wrap has been replaced with vl_encode_facet and will be removed in future package versions")
  vl_encode_facet(...)
}
