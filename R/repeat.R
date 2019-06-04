.add_repeat <- function(spec, ...) {
  UseMethod(".add_repeat", spec)
}

.add_repeat.vegaspec_repeat <- function(spec, .type, ...) {
  
  if (.type == "row") {
    spec[["repeat"]][["row"]] <- list(...)
  } else if (.type == "col") {
    spec[["repeat"]][["col"]] <- list(...)
  } else{
    repeat_def <- list(...)
    columns <- repeat_def$columns
    repeat_def$columns <- NULL
    names(repeat_def) <- NULL
    spec[["repeat"]] <- repeat_def
    spec[["columns"]] <- columns
  }
  
  return(spec)
}


.add_repeat.vegaspec_unit <- function(spec, .type, ...) {
  
  keep <- c('$schema','data')
  move <- names(spec)[which(!(names(spec) %in% keep))]
  old_spec <- spec
  spec <- spec[keep]
  spec$spec <- old_spec[move]
  
  if (.type == "row") {
    spec[["repeat"]][["row"]] <- list(...)
  } else if (.type == "col") {
    spec[["repeat"]][["col"]] <- list(...)
  } else{
    repeat_def <- list(...)
    columns <- repeat_def$columns
    repeat_def$columns <- NULL
    names(repeat_def) <- NULL
    spec[["repeat"]] <- repeat_def
    spec[["columns"]] <- columns
  }
  
  return(vegawidget::as_vegaspec(spec))
}