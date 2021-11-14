.add_parameter <- function(spec, ...) {
  UseMethod(".add_parameter", spec)
}

.add_parameter.vegaspec_layer <- function(spec, ...) {
  stop("Cant' add parameter to layered spec")
}

.add_parameter.vegaspec_vega_lite <- function(spec, obj, ref) {
  fn <- function(spec) {
    if (!hasName(spec, "params")) spec$params <- list()
    validate_sub_schema(obj, ref)
    spec[["params"]] <- c(spec[["params"]], list(obj))
    spec
  }
  modify_inner_spec(spec, fn)
}


.add_selection <- function(spec, ...) {
  UseMethod(".add_selection", spec)
}

.add_selection.vegaspec_layer <- function(spec, ...) {
  stop("Cant' add selection to layered spec")
}

.add_selection.vegaspec_vega_lite <- function(spec, obj, ref) {
  obj_names <- names(obj)
  top_names <- c("name","bind","value","select")
  other_names <- setdiff(obj_names, top_names)
  select_subset <- obj[which(obj_names %in% other_names)]
  obj[which(obj_names %in% other_names)] <- NULL
  if (!hasName(obj, "select")) obj[["select"]] <- list()
  obj[["select"]] <- c(obj[["select"]], select_subset)
  
  .add_parameter(spec, obj, ref)
}



.add_binding <- function(spec, ...) {
  UseMethod(".add_binding", spec)
}

.add_binding.vegaspec_layer <- function(spec, ...) {
  stop("Can't add parameter binding to layered spec")
}

.get_names_helper <- function(x) {
  vapply(x, function(y) y[['name']], "")
}

.add_binding.vegaspec_vega_lite <- function(spec, obj, ref, parameter_name, projection_name = NULL) {
  fn <- function(spec) {
    if (!hasName(spec, "params") ||  !(parameter_name %in% .get_names_helper(spec[["params"]])) ) {
      stop("Can't add binding to parameter that does not exist")
    }

    validate_sub_schema(obj, ref)

    if (is.null(projection_name)) {
      binding <- obj
    } else {
      binding <- list()
      binding[[projection_name]] <- obj
    }

    param_index <- match(parameter_name, .get_names_helper(spec[["params"]]))
    
    
    if (hasName(spec[["params"]][[param_index]], "bind")) {
      if (is.null(projection_name)) {
        stop("If adding multiple bindings to single parameter, must name each")
      }
      # check if previous is named?
      spec[["params"]][[param_index]][["bind"]] <- c(spec[["params"]][[param_index]][["bind"]], binding)
    } else {
      spec[["params"]][[param_index]][["bind"]] <- binding
    }
    spec
  }
  modify_inner_spec(spec, fn)
}
