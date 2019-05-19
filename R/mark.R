
.add_mark <- function(spec, .mark = NULL, ...){

  if (is_composite(spec)) {
    stop("Can't add mark to composite spec; add prior to combining specs")
  }
  if (hasName(spec, "spec")) {
    inner_spec <- TRUE
    outer_spec <- spec
    spec <- outer_spec[["spec"]]
  } else {
    inner_spec <- FALSE
  }

  def <- list(...)
  if (length(def) == 0){
    spec[["mark"]] <- .mark
  } else {
    if (!is.null(.mark)) def$type <- .mark
    spec[["mark"]] <- def
  }

  if (inner_spec) {
    outer_spec[["spec"]] <- spec
    spec <- outer_spec
  }
  return(spec)
}
