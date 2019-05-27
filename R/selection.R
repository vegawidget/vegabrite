.add_selection <- function(spec, selection_name, ...){
  
  if (is_composite(spec)) {
    stop("Can't add selection to composite spec; add selection prior to combining specs")
  }
  if (hasName(spec, "spec")) {
    inner_spec <- TRUE
    outer_spec <- spec
    spec <- outer_spec[["spec"]]
  } else {
    inner_spec <- FALSE
  }
  
  if (!hasName(spec,"selection")) spec$selection <- list()
  new_sel <- list()
  new_sel[[selection_name]] <- list(...)
  spec[["selection"]] <- c(spec[["selection"]],new_sel)
  
  if (inner_spec) {
    outer_spec[["spec"]] <- spec
    spec <- outer_spec
  }
  return(spec)
}

.add_binding <- function(spec, selection_name, projection_name = NULL, ...){
  
  if (is_composite(spec)) {
    stop("Can't add binding to composite spec; add transform prior to combining specs")
  }
  if (hasName(spec, "spec")) {
    inner_spec <- TRUE
    outer_spec <- spec
    spec <- outer_spec[["spec"]]
  } else {
    inner_spec <- FALSE
  }
  
  if (!hasName(spec,"selection") || !hasName(spec$selection, selection_name)) {
    stop("Can't add binding to selection that does not exist")
  }
  
  if (is.null(projection_name)){
    binding <- list(...)
  } else{
    binding <- list()
    binding[[projection_name]] <- list(...)
  }
  
  if (hasName(spec[["selection"]][[selection_name]],"bind")){
    if (is.null(projection_name)) {
      stop("If adding multiple bindings to single selection, must name each")
    }    
    # check if previous is named?
    spec[["selection"]][[selection_name]][["bind"]] <- c(spec[["selection"]][[selection_name]][["bind"]], binding)
  } else {
    spec[["selection"]][[selection_name]][["bind"]] <- binding
  }

  if (inner_spec) {
    outer_spec[["spec"]] <- spec
    spec <- outer_spec
  }
  return(spec)
}
