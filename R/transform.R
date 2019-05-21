.add_transform <- function(spec, .trans, ...){
  
  if (is_composite(spec)) {
    stop("Can't add transform to composite spec; add transform prior to combining specs")
  }
  if (hasName(spec, "spec")) {
    inner_spec <- TRUE
    outer_spec <- spec
    spec <- outer_spec[["spec"]]
  } else {
    inner_spec <- FALSE
  }
  
  if (!hasName(spec,"transform")) spec$transform <- list()
  spec[["transform"]] <- c(spec[["transform"]],list(list(...)))
  
  if (inner_spec) {
    outer_spec[["spec"]] <- spec
    spec <- outer_spec
  }
  return(spec)
}