#' Deprecated functions from vlbuildr
#'
#' These functions have been deprecated in favor of a different
#' naming convention.
#' @param ... argument to pass to new function
#' @name vlbuildr-deprecated
NULL

#' @rdname vlbuildr-deprecated
vl_facet_wrap <- function(...) {
  .Deprecated('vl_facet', package = 'vlbuildr', "vl_facet_wrap has been replaced with vl_facet")
  vl_facet(...)
}

#' @rdname vlbuildr-deprecated
vl_encode_wrap <- function(...) {
  .Deprecated('vl_encode_facet', package = 'vlbuildr', "vl_encode_wrap has been replaced with vl_encode_facet")
  vl_facet(...)
}