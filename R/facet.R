.add_facet <- function(spec, .type, ...){
  
  if (is_composite(spec)) {
    stop("Can't add transform to composite spec; add transform prior to combining specs")
  }
  if (!hasName(spec, "spec")) {
    keep <- c('$schema','data')
    move <- names(spec)[which(!(names(spec) %in% keep))]
    old_spec <- spec
    spec <- vegawidget::as_vegaspec(spec[keep])
    spec$spec <- old_spec[move]
  }

  if (.type == "row") {
    spec[["facet"]][["row"]] <- list(...)
  } else if (.type == "col") {
    spec[["facet"]][["col"]] <- list(...)
  } else{
    facet_def <- list(...)
    columns <- facet_def$columns
    facet_def$columns <- NULL
    spec[["facet"]] <- facet_def
    spec[["columns"]] <- columns
  }
  
  return(spec)
}

.add_facet_row <- function(spec, ...){
  .add_facet(spec, "row", ...)
}

.add_facet_col <- function(spec, ...){
  .add_facet(spec, "col", ...)
}

.add_facet_wrap <- function(spec, ...){
  .add_facet(spec, "wrap", ...)
}