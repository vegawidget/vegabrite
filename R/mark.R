.add_mark <- function(spec, ...) {
  UseMethod(".add_mark", spec)
}

.add_mark.default <- function(spec, obj){
  .add_to_inner_spec(spec, obj, "mark", how = "replace")
}

.add_mark.vegaspec_unit <- function(spec, obj){
  
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
    return(.add_mark(vegawidget::as_vegaspec(spec), obj))
  } 
  
  .add_to_inner_spec(spec, obj, "mark", how = "replace")

  }


.add_mark.vegaspec_layer <- function(spec, obj){
  
  new_spec <- list("mark" = obj)
  spec$layer <- c(spec$layer, list(new_spec))
  
  return(vegawidget::as_vegaspec(spec))
}