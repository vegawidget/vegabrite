TOP_LEVEL_KEYS <- c(
  "$schema", "autosize", "background", "config", "datasets",
  "padding", "usermeta"
)

.modify_args <- function(override, exclude) {
  ## Capture inputs provided by user and make into an object
  args_in <- rlang::fn_fmls_syms(rlang::caller_fn(n = 1))
  args_eval <- lapply(args_in, eval, env = rlang::caller_env(n = 1))
  args_nn <- args_eval[!vapply(args_eval, is.null, FALSE)]
  args_nn <- c(args_nn, override)
  is_obj <- !(names(args_nn) %in% c(exclude, ".object", "spec"))
  ## If .object is provided, use that instead...
  if (".object" %in% names(args_nn)) {
    if (sum(is_obj) > 0) {
      # Get the name of first arg...
      first_obj_arg <- names(args_in)[!(names(args_in) %in% c(exclude, ".object", "spec"))][1]
      if (!hasName(args_nn, first_obj_arg)) {
        args_obj <- args_nn[is_obj]
        args_obj[[first_obj_arg]] <- args_nn[[".object"]]
      } else {
        fn <- sys.calls()[[sys.nframe() - 1]][[1]]
        stop("Error in arguments to ",
          fn,
          ".\n  When passing arguments to make a new object to add to the spec other than .object",
          ", if .object is passed as well it is treated as the first other argument, which in this",
          " case was already provided. See help('vlbuidlr').",
          call. = FALSE
        )
      }
    } else {
      args_obj <- args_nn[[".object"]]
    }
  } else {
    args_obj <- args_nn[is_obj]
  }

  args_obj
}

.add_to_top_spec <- function(spec, x, name, ref,
                             how = c("replace", "append", "match")) {
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
                               how = c("replace", "append", "match")) {
  how <- match.arg(how)

  fn <- function(spec) {
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
  keep <- intersect(TOP_LEVEL_KEYS, names(spec))
  move <- names(spec)[which(!(names(spec) %in% keep))]
  list(outer = spec[keep], inner = spec[move])
}

.extract_inner_specs <- function(...) {
  modified <- lapply(list(...), .extract_inner_spec)
  inners <- lapply(modified, function(x) x[["inner"]])
  outers <- lapply(modified, function(x) x[["outer"]])
  list(inners = inners, outers = outers)
}

.get_inline_data <- function(spec) {
  if (!hasName(spec, "data") || !hasName(spec$data, "values")) {
    NULL
  } else {
    spec$data$value
  }
}
