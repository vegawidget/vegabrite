.add_repeat <- function(spec, .type, ...) {
  
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
