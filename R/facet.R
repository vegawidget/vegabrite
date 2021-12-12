.add_facet <- function(spec, ...) {
  UseMethod(".add_facet", spec)
}

.facet_sugar <- function(obj, spec) {
  if (!is.list(obj) && length(obj) == 1) {
    obj <- list(field = obj[[1]])
  }
  obj <- .type_sugar(obj, spec)
  obj <- .escape_dot(obj, "field")
  obj
}

.add_facet.vegaspec_facet <- function(spec, obj, ref, .type, columns = NULL) {
  if (.type == "wrap" && is.null(columns)) {
    columns <- obj$columns
    obj$columns <- NULL
  }
  obj <- .facet_sugar(obj, spec)
  validate_sub_schema(obj, ref)
  if (.type == "row") {
    spec[["facet"]][["row"]] <- obj
  } else if (.type == "column") {
    spec[["facet"]][["column"]] <- obj
  } else {
    spec[["facet"]] <- obj
    spec[["columns"]] <- columns
  }

  return(vegawidget::as_vegaspec(spec))
}

.add_facet.vegaspec_layer <- function(spec, obj, ref, .type, columns = NULL) {
  keep <- c("$schema", "data")
  keep <- keep[which(keep %in% names(spec))]
  move <- names(spec)[which(!(names(spec) %in% keep))]
  old_spec <- spec
  spec <- spec[keep]
  spec$spec <- old_spec[move]
  
  if (.type == "wrap" && is.null(columns)) {
    columns <- obj$columns
    obj$columns <- NULL
  }
  obj <- .facet_sugar(obj, spec)
  validate_sub_schema(obj, ref)
  if (.type == "row") {
    spec[["facet"]][["row"]] <- obj
  } else if (.type == "column") {
    spec[["facet"]][["column"]] <- obj
  } else {
    spec[["facet"]] <- obj
    spec[["columns"]] <- columns
  }
  
  return(vegawidget::as_vegaspec(spec))
}

.add_facet.vegaspec_unit <- function(spec, obj, ref, .type, columns = NULL) {
  keep <- c("$schema", "data")
  keep <- keep[which(keep %in% names(spec))]
  move <- names(spec)[which(!(names(spec) %in% keep))]
  old_spec <- spec
  spec <- spec[keep]
  spec$spec <- old_spec[move]

  if (.type == "wrap" && is.null(columns)) {
    columns <- obj$columns
    obj$columns <- NULL
  }
  obj <- .facet_sugar(obj,spec)
  validate_sub_schema(obj, ref)
  if (.type == "row") {
    spec[["facet"]][["row"]] <- obj
  } else if (.type == "column") {
    spec[["facet"]][["column"]] <- obj
  } else {
    spec[["facet"]] <- obj
    spec[["columns"]] <- columns
  }

  return(vegawidget::as_vegaspec(spec))
}

.add_facet_row <- function(spec, ...) {
  .add_facet(spec, ..., .type = "row")
}

.add_facet_column <- function(spec, ...) {
  .add_facet(spec, ..., .type = "column")
}

.add_facet_wrap <- function(spec, ...) {
  .add_facet(spec, ..., .type = "wrap")
}
