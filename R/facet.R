add_facet <- function(spec, ...) {
  UseMethod("add_facet", spec)
}



add_facet.vegaspec_facet <- function(spec, .type, ...) {

  if (.type == "row") {
    spec[["facet"]][["row"]] <- list(...)
  } else if (.type == "column") {
    spec[["facet"]][["column"]] <- list(...)
  } else{
    facet_def <- list(...)
    columns <- facet_def$columns
    facet_def$columns <- NULL
    spec[["facet"]] <- facet_def
    spec[["columns"]] <- columns
  }
  
  return(vegawidget::as_vegaspec(spec))
}

add_facet.vegaspec_unit <- function(spec, .type, ...) {
  
  keep <- c('$schema','data')
  move <- names(spec)[which(!(names(spec) %in% keep))]
  old_spec <- spec
  spec <- spec[keep]
  spec$spec <- old_spec[move]
  
  if (.type == "row") {
    spec[["facet"]][["row"]] <- list(...)
  } else if (.type == "column") {
    spec[["facet"]][["column"]] <- list(...)
  } else{
    facet_def <- list(...)
    columns <- facet_def$columns
    facet_def$columns <- NULL
    spec[["facet"]] <- facet_def
    spec[["columns"]] <- columns
  }
  
  return(vegawidget::as_vegaspec(spec))
}


.add_facet_row <- function(spec, ...){
  add_facet(spec, "row", ...)
}

.add_facet_column <- function(spec, ...){
  add_facet(spec, "column", ...)
}

.add_facet_wrap <- function(spec, ...){
  add_facet(spec, "wrap", ...)
}