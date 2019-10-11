.add_facet <- function(spec, ...) {
  UseMethod(".add_facet", spec)
}



.add_facet.vegaspec_facet <- function(spec, obj, ref, .type, columns) {

  validate_sub_schema(obj, ref)
  if (.type == "row") {
    spec[["facet"]][["row"]] <- obj
  } else if (.type == "column") {
    spec[["facet"]][["column"]] <- obj
  } else{
    spec[["facet"]] <- obj
    spec[["columns"]] <- columns
  }
  
  return(vegawidget::as_vegaspec(spec))
}

.add_facet.vegaspec_unit <- function(spec, obj, ref, .type, columns) {
  
  keep <- c('$schema','data')
  move <- names(spec)[which(!(names(spec) %in% keep))]
  old_spec <- spec
  spec <- spec[keep]
  spec$spec <- old_spec[move]
  
  validate_sub_schema(obj, ref)
  if (.type == "row") {
    spec[["facet"]][["row"]] <- obj
  } else if (.type == "column") {
    spec[["facet"]][["column"]] <- obj
  } else{
    spec[["facet"]] <- obj
    spec[["columns"]] <- columns
  }
  
  return(vegawidget::as_vegaspec(spec))
}


.add_facet_row <- function(spec, ...){
  .add_facet(spec, ..., .type = "row")
}

.add_facet_column <- function(spec, ...){
  .add_facet(spec, ..., .type = "column")
}

.add_facet_wrap <- function(spec, ...){
  .add_facet(spec, ..., .type = "wrap")
}