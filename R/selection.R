.add_selection <- function(spec, selection_name, ...){
  
  fn <- function(spec){
    if (!hasName(spec,"selection")) spec$selection <- list()
    new_sel <- list()
    new_sel[[selection_name]] <- list(...)
    spec[["selection"]] <- c(spec[["selection"]],new_sel)
    spec
  }
  modify_inner_spec(spec, fn)
}

.add_binding <- function(spec, selection_name, projection_name = NULL, ...){
  
  fn <- function(spec){
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
    spec
  }
  modify_inner_spec(spec, fn)
}
