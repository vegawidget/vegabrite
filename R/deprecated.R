#' Deprecated functions from vlr
#'
#' These functions have been deprecated in favor of a different
#' naming convention.
#' @param ... argument to pass to new function
#' @name vlr-deprecated
NULL

#' @rdname vlr-deprecated
#' @export
vl_facet_wrap <- function(...) {
  .Deprecated('vl_facet', package = 'vlr', "vl_facet_wrap has been replaced with vl_facet and will be removed in future package versions")
  vl_facet(...)
}

#' @rdname vlr-deprecated
#' @export
vl_encode_wrap <- function(...) {
  .Deprecated('vl_encode_facet', package = 'vlr', "vl_encode_wrap has been replaced with vl_encode_facet and will be removed in future package versions")
  vl_encode_facet(...)
}
