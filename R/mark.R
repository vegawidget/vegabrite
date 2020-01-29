.add_mark <- function(spec, ...) {
  UseMethod(".add_mark", spec)
}

.mark_sugar <- function(obj) {
  # Sugar for tooltip setting to encoding/data
  if (hasName(obj, "tooltip") && obj["tooltip"] %in% c("encoding", "data")) {
    obj[["tooltip"]] <- list(content = obj[["tooltip"]])
  }
  obj
}

.add_mark.default <- function(spec, obj, ref, ...) {
  obj <- .mark_sugar(obj)
  .add_to_inner_spec(spec, obj, "mark", ref, how = "replace")
}

.add_mark.vegaspec_unit <- function(spec, obj, ref, ...) {
  obj <- .mark_sugar(obj)
  if (hasName(spec, "mark")) {
    if (hasName(spec, "selection")) {
      stop("Can't add additional marks to spec with selection. Use vl_layer to add layers.")
    }
    message(
      "Adding additional mark by making spec layered; may limit",
      " further modification.",
      " Use vl_layer instead for more flexibility."
    )
    # Move mark inside a layer
    old_mark <- spec$mark
    spec$mark <- NULL
    spec$layer <- list(list(mark = old_mark))
    return(.add_mark(vegawidget::as_vegaspec(spec), obj))
  }

  .add_to_inner_spec(spec, obj, "mark", ref, how = "replace")
}


.add_mark.vegaspec_layer <- function(spec, obj, ref, ...) {
  obj <- .mark_sugar(obj)
  new_spec <- list("mark" = obj)
  spec$layer <- c(spec$layer, list(new_spec))

  return(vegawidget::as_vegaspec(spec))
}
