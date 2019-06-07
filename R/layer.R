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
  
  out_spec <- purrr::reduce(parts$outers, utils::modifyList, .dir = "backward")
  
  out_spec[["layer"]] <- parts$inners
  
  return(vegawidget::as_vegaspec(out_spec))
  
}