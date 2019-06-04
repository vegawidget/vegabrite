
is_composite <- function(spec){

  if (hasName(spec, "spec")) {
    any(c("vconcat","hconcat","concat","layer") %in% names(spec[["spec"]]))
  } else{
    any(c("vconcat","hconcat","concat","layer") %in% names(spec))
  }

}

modify_inner_spec <- function(spec, ...) {
  UseMethod("modify_inner_spec", spec)
}

modify_inner_spec.vegaspec_concat <- function(spec, fn) {
  stop("Can't apply change to a concat spec; apply prior to concatenating")
}

modify_inner_spec.vegaspec_hconcat <- function(spec, fn) {
  stop("Can't apply change to a hconcat spec; apply prior to concatenating")
}

modify_inner_spec.vegaspec_vconcat <- function(spec, fn) {
  stop("Can't apply change to a vconcat spec; apply prior to concatenating")
}

modify_inner_spec.vegaspec_layer <- function(spec, fn) {
  stop("Can't apply change to a layer spec; apply prior to concatenating")
}


modify_inner_spec.vegaspec_repeat <- function(spec, fn) {
  spec$spec <- fn(spec$spec)
  spec
}

modify_inner_spec.vegaspec_facet <- function(spec, fn) {
  spec$spec <- fn(spec$spec)
  spec
}

modify_inner_spec.vegaspec_unit <- function(spec, fn) {
  fn(spec)
}



