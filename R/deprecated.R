#' Deprecated functions from vlbuildr
#'
#' These functions have been deprecated in favor of a different
#' naming convention.
#' @param ... argument to pass to new function
#' @name vlbuildr-deprecated
NULL

#' @rdname vlbuildr-deprecated
#' @export
vl_facet_wrap <- function(...) {
  .Deprecated('vl_facet', package = 'vlbuildr', "vl_facet_wrap has been replaced with vl_facet and will be removed in future package versions")
  vl_facet(...)
}

#' @rdname vlbuildr-deprecated
#' Export
vl_encode_wrap <- function(...) {
  .Deprecated('vl_encode_facet', package = 'vlbuildr', "vl_encode_wrap has been replaced with vl_encode_facet and will be removed in future package versions")
  vl_facet(...)
}