#' Layering functions
#'
#' @param ... specs
#'
#' @return A composite spec
#' @export
#'
#' @name layer
vl_layer <- function(...) {
  
  parts <- .extract_inner_specs(...)
  
  out_spec <- Reduce(utils::modifyList, parts$outers, right = TRUE)
  
  out_spec[["layer"]] <- parts$inners
  
  return(vegawidget::as_vegaspec(out_spec))
  
}