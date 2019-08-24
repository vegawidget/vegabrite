TOP_LEVEL_KEYS <- c("$schema", "autosize", "background", "config", "datasets", 
  "padding", "usermeta")


.modify_args <- function(override, valid_names) {
  args_in <- rlang::fn_fmls_syms(rlang::caller_fn(n = 1))
  args_eval <- lapply(args_in, eval, env = rlang::caller_env(n = 1))
  args_nn <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_nn <- c(args_nn, override)
  is_obj <- names(args_nn) %in% valid_names
  args_obj <- args_nn[is_obj]
  args_extra <- setdiff(args_nn[!is_obj], c("spec"))
  list(object = args_obj, extra = args_extra, spec = args_nn[['spec']])
}


.add_to_top_spec <- function(spec, x, name, ref,
                          how = c("replace","append","match")) {

  how <- match.arg(how)
  
  res <- validate_sub_schema(x, ref)

  if (how == "replace") {
    spec[[name]] <- x
  } else if (how == "append") {
    if (!hasName(spec, name)) {
      spec[[name]] <- list()
    }
    spec[[name]] <- c(spec[[name]], list(x))
  } else {
    spec[[name]] <- modifyList(spec[[name]], x)
  }

  spec
}

.add_to_inner_spec <- function(spec, x, name, ref,
                                 how = c("replace","append","match")) {
  
  how <- match.arg(how)
  
  fn <- function(spec){
    .add_to_top_spec(spec, x, name, ref, how)
  }
  
  modify_inner_spec(spec, fn)
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
  # Exceptions: mark & selection
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

