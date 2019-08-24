.add_selection <- function(spec, ...) {
  UseMethod(".add_selection", spec)
}

.add_selection.vegaspec_layer <- function(spec, ...) {
  stop("Cant' add selection to layered spec")  
}

.add_selection.vegaspec_vega_lite <- function(spec, obj, ref, selection_name, type){
  
  fn <- function(spec){
    if (!hasName(spec,"selection")) spec$selection <- list()
    new_sel <- list()
    obj$type <- type
    validate_sub_schema(obj, ref)
    new_sel[[selection_name]] <- obj
    spec[["selection"]] <- c(spec[["selection"]],new_sel)
    spec
  }
  modify_inner_spec(spec, fn)
}

.add_binding <- function(spec, ...) {
  UseMethod(".add_binding", spec)
}

.add_binding.vegaspec_layer <- function(spec, ...) {
  stop("Can't add selection binding to layered spec")
}

.add_binding.vegaspec_vega_lite <- function(spec, obj, ref, selection_name, projection_name = NULL){
  
  fn <- function(spec){
    if (!hasName(spec,"selection") || !hasName(spec$selection, selection_name)) {
      stop("Can't add binding to selection that does not exist")
    }
    
    validate_sub_schema(obj, ref)
    
    if (is.null(projection_name)){
      binding <- obj
    } else {
      binding <- list()
      binding[[projection_name]] <- obj
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
