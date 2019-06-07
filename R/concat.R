
#' Concatenation functions
#'
#' @param ... specs
#'
#' @return A composite spec
#' @export
#'
#' @name concat
vl_hconcat <- function(...) {

  parts <- .extract_inner_specs(...)
  
  out_spec <- purrr::reduce(parts$outers, utils::modifyList, .dir = "backward")
  
  out_spec[["hconcat"]] <- parts$inners
   
  return(vegawidget::as_vegaspec(out_spec))
  
}

#' @export
#'
#' @name concat
vl_vconcat <- function(...) {
  
  parts <- .extract_inner_specs(...)
  
  out_spec <- purrr::reduce(parts$outers, utils::modifyList, .dir = "backward")
  
  out_spec[["vconcat"]] <- parts$inners
  
  return(vegawidget::as_vegaspec(out_spec))
  
}

#' @export
#'
#' @name concat
vl_concat <- function(..., columns = 2) {
  
  parts <- .extract_inner_specs(...)
  
  out_spec <- purrr::reduce(parts$outers, utils::modifyList, .dir = "backward")
  
  out_spec[["concat"]] <- parts$inners
  out_spec[["columns"]] <- columns
  
  return(vegawidget::as_vegaspec(out_spec))
  
}

