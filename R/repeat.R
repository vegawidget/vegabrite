.add_repeat <- function(spec, ...) {
  UseMethod(".add_repeat", spec)
}

.add_repeat.vegaspec_repeat <- function(spec, obj, ref, .type, columns = 2) {
  if (.type == "row") {
    spec[["repeat"]][["row"]] <- obj
  } else if (.type == "col") {
    spec[["repeat"]][["column"]] <- obj
  } else if (.type == "layer") {
    spec[["repeat"]][["layer"]] <- obj
  } else {
    repeat_def <- obj
    repeat_def$columns <- NULL
    names(repeat_def) <- NULL
    spec[["repeat"]] <- repeat_def
    spec[["columns"]] <- columns
  }

  return(spec)
}


.add_repeat.vegaspec_unit <- function(spec, obj, ref, .type, columns = 2) {
  keep <- c("$schema", "data")
  move <- names(spec)[which(!(names(spec) %in% keep))]
  old_spec <- spec
  spec <- spec[keep]
  spec$spec <- old_spec[move]

  if (.type == "row") {
    spec[["repeat"]][["row"]] <- obj
  } else if (.type == "col") {
    spec[["repeat"]][["column"]] <- obj
  } else if (.type == "layer") {
    spec[["repeat"]][["layer"]] <- obj
  } else {
    repeat_def <- obj
    repeat_def$columns <- NULL
    names(repeat_def) <- NULL
    spec[["repeat"]] <- repeat_def
    spec[["columns"]] <- columns
  }

  return(vegawidget::as_vegaspec(spec))
}
