.add_mark <- function(spec, ...) {
  UseMethod(".add_mark", spec)
}

.add_mark.default <- function(spec, .mark = NULL, ...){

  fn <- function(spec) {
    def <- list(...)
    if (length(def) == 0){
      spec[["mark"]] <- .mark
    } else {
      if (!is.null(.mark)) def$type <- .mark
      spec[["mark"]] <- def
    }
    spec
  }
  
  modify_inner_spec(spec, fn)
  
  return(spec)
}

.add_mark.vegaspec_unit <- function(spec, .mark = NULL, ...){
  
  if (hasName(spec,"mark")) {
    if (hasName(spec, "selection")) {
      stop("Can't add additional marks to spec with selection. Use vl_layer to add layers.")
    }
    message("Adding additional mark by making spec layered; may limit",
            " further modification.", 
            " Use vl_layer instead for more flexibility.")
    # Move mark inside a layer
    old_mark <- spec$mark
    spec$mark <- NULL
    spec$layer <- list(list(mark = old_mark))
    return(.add_mark(vegawidget::as_vegaspec(spec), .mark, ...))
  } 
  
  def <- list(...)
  if (length(def) == 0) {
    spec[["mark"]] <- .mark
  } else {
    if (!is.null(.mark)) def$type <- .mark
    spec[["mark"]] <- def
  }
  
  return(spec)
}


.add_mark.vegaspec_layer <- function(spec, .mark = NULL, ...){
  
  new_spec <- list()
  def <- list(...)
  if (length(def) == 0) {
    new_spec[["mark"]] <- .mark
  } else {
    if (!is.null(.mark)) def$type <- .mark
    new_spec[["mark"]] <- def
  }
  
  spec$layer <- c(spec$layer, list(new_spec))
  
  return(vegawidget::as_vegaspec(spec))
}