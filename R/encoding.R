TYPE_MAPPING <- list(
  "N" = "nominal",
  "T" = "temporal",
  "Q" = "quantitative",
  "O" = "ordinal"
)

.type_sugar <- function(obj, spec) {
  if (!hasName(obj, "type") && hasName(obj, "field") && grepl(":[N,O,Q,T]$", obj[["field"]])) {
    field <- obj$field
    nc <- nchar(field)
    obj$type <- TYPE_MAPPING[[substr(field, nc, nc)]]
    obj$field <- substr(field, 1, nc - 2)
  } else if (hasName(obj, "type") && obj$type %in% names(TYPE_MAPPING)) {
    obj$type <- TYPE_MAPPING[[obj$type]]
  } else if (!hasName(obj, "type") && hasName(obj, "field")) {
    dat <- .get_inline_data(spec)
    if (!is.null(dat)) {
      col <- dat[[obj$field]]
      if (is.factor(col)) {
        obj$type <- "ordinal"
      } else if (inherits(col, c("POSIXt", "POSIXct", "POSIXlt", "Date"))) {
        obj$type <- "temporal"
      } else if (is.numeric(col)) {
        obj$type <- "quantitative"
      } else {
        obj$type <- "nominal"
      }
    }
  }
  obj
}

.escape_dot <- function(obj, element = "field") {
  if (hasName(obj, element) && !is.list(obj[[element]])) {
    obj[[element]]<- gsub("\\.", "\\\\.", obj[[element]])
  }
  obj
}

.repeat_sugar <- function(obj) {
  if (hasName(obj, "field") && grepl("^repeat:(column|row|repeat)$", obj[["field"]])) {
    obj[["field"]] <- list(`repeat` = substr(obj$field, 8, nchar(obj$field)))
  } else if (hasName(obj, "field") && (obj$field == "repeat:" || obj$field == "repeat:wrap")) {
    obj[["field"]] <- list(`repeat` = "repeat")
  }
  obj
}

.encoding_sugar <- function(obj, spec) {
  obj <- .type_sugar(obj, spec)
  obj <- .repeat_sugar(obj)
  obj <- .escape_dot(obj, "field")
  obj
}


.add_encoding <- function(spec, obj, ref, encoding) {
  fn <- function(spec) {

    # Handle shorthand
    enc <- obj
    if (!is.list(enc)) {
      if (length(enc) == 1) {
        enc <- .encoding_sugar(list(field = enc[[1]]), spec)
      } else {
        enc <- lapply(enc, function(x) .encoding_sugar(list(field = x), spec))
      }
    } else {
      if (!is.null(names(enc))) {
        enc <- .encoding_sugar(enc, spec) 
      } else {
        enc <- lapply(enc, function(x) .encoding_sugar(x, spec))
      }
    }

    if (!hasName(spec, "encoding")) spec$encoding <- list()
    validate_sub_schema(enc, ref)
    spec[["encoding"]][[encoding]] <- enc
    spec
  }

  modify_inner_spec(spec, fn)
}

.add_param_to_encoding <- function(spec, obj, ref, encoding, param, ...) {
  fn <- function(spec) {
    if (!hasName(spec, "encoding") || !hasName(spec[["encoding"]], encoding)) {
      stop(
        "Error in adding ", param, " to ", encoding,
        "\nCould not find ", encoding, " encoding in spec.",
        "\nAdd encoding first before adding, ", param, "."
      )
    }
    validate_sub_schema(obj, ref)
    spec[["encoding"]][[encoding]][[param]] <- obj
    spec
  }

  modify_inner_spec(spec, fn)
}

.add_sort_to_encoding <- function(spec, obj, ref, encoding, ...) {
  .add_param_to_encoding(spec, obj, ref, encoding, param = "sort", ...)
}


.add_axis_to_encoding <- function(spec, obj, ref, encoding, ...) {
  .add_param_to_encoding(spec, obj, ref, encoding, param = "axis", ...)
}

.add_scale_to_encoding <- function(spec, obj, ref, encoding, ...) {
  .add_param_to_encoding(spec, obj, ref, encoding, param = "scale", ...)
}

.add_legend_to_encoding <- function(spec, obj, ref, encoding, ...) {
  .add_param_to_encoding(spec, obj, ref, encoding, param = "legend", ...)
}

.add_condition_to_encoding <- function(spec, obj, ref, encoding, ...) {

  # adds to an array, so not use standard func...

  fn <- function(spec) {
    if (!hasName(spec, "encoding") || !hasName(spec[["encoding"]], encoding)) {
      stop(
        "Error in adding condition to ", encoding,
        "\nCould not find ", encoding, " encoding in spec.",
        "\nAdd encoding first before adding condition."
      )
    }

    validate_sub_schema(obj, ref)

    if (hasName(spec[["encoding"]][[encoding]], "condition")) {
      # Check if named
      if (is.null(names(spec[["encoding"]][[encoding]][["condition"]]))) {
        value <- c(spec[["encoding"]][[encoding]][["condition"]], list(obj))
      } else {
        value <- c(list(spec[["encoding"]][[encoding]][["condition"]]), list(obj))
      }
    } else {
      value <- obj
    }

    spec[["encoding"]][[encoding]][["condition"]] <- value
    spec
  }

  modify_inner_spec(spec, fn)
}

#' vl_encode
#'
#' Add encodings to a spec. This is one way to add encodings... each encoding
#' can also be added using vl_encode_<name> where <name> is the name of the
#' encoding.
#'
#' @param spec a vegalite spec
#' @param ... encodings to add. Each encoding should be a list. See example.
#'
#' @return a modified vegalite spec
#' @export
#'
#' @examples
#'
#' vl_chart() %>%
#'   vl_add_data(values = mtcars) %>%
#'   vl_mark_point() %>%
#'   vl_encode(
#'     x = "wt:Q",
#'     y = "mpg:Q"
#'   )
vl_encode <- function(spec, ...) {
  inputs <- list(...)
  for (n in names(inputs)) {
    if (n %in% c("row", "column", "facet")) {
      args <- list(spec = spec, obj = inputs[[n]], ref = "#/definitions/FacetEncodingFieldDef", encoding = n)
    } else {
      args <- list(spec = spec, obj = inputs[[n]], ref = paste0("#/definitions/Encoding/properties/", n), encoding = n)
    }
    spec <- do.call(.add_encoding, args)
  }
  spec
}
