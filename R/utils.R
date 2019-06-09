TOP_LEVEL_KEYS <- c("$schema", "autosize", "background", "config", "datasets", 
  "padding", "usermeta")

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
  # Layer spec can have most 'inner' spec properties...
  # Excpetions: mark & selection
  # Functions calling this helper should have a separate
  # layer spec function if needed...
  fn(spec)
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


.extract_inner_spec <- function(spec) {
  keep <- intersect(TOP_LEVEL_KEYS,names(spec))
  move <- names(spec)[which(!(names(spec) %in% keep))]
  list(outer = spec[keep], inner = spec[move])
}

.extract_inner_specs <- function(...) {
  modified <- lapply(list(...), .extract_inner_spec)
  inners <- lapply(modified, function(x) x[['inner']])
  outers <- lapply(modified, function(x) x[['outer']])
  list(inners = inners, outers = outers)
}

.get_inline_data <- function(spec) {
  if (!hasName(spec,"data") || !hasName(spec$data, "values")){
    NULL
  } else{
    spec$data$value
  }
}

