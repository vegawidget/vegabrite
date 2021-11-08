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

.add_binding <- function(spec, ...) {
  UseMethod(".add_binding", spec)
}

.add_binding.vegaspec_layer <- function(spec, ...) {
  stop("Can't add selection binding to layered spec")
}

.add_binding.vegaspec_vega_lite <- function(spec, obj, ref, selection_name, projection_name = NULL) {
  fn <- function(spec) {
    if (!hasName(spec, "selection") || !hasName(spec$selection, selection_name)) {
      stop("Can't add binding to selection that does not exist")
    }

    validate_sub_schema(obj, ref)

    if (is.null(projection_name)) {
      binding <- obj
    } else {
      binding <- list()
      binding[[projection_name]] <- obj
    }

    if (hasName(spec[["selection"]][[selection_name]], "bind")) {
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
