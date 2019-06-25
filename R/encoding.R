ENCODE_MAPPING <- list(
  "N" = "nominal",
  "T" = "temporal",
  "Q" = "quantitative",
  "O" = "ordinal"
)

.add_encoding <- function(spec, .enc, ...){

  fn <- function(spec) {
    
    # Handle shorthand
    enc <- list(...)
    if (is.null(names(enc)) && length(enc) == 1) {
      enc <- list(field = enc[[1]])
    }
    if (!hasName(enc,"type") && hasName(enc,"field") && grepl(":[N,O,Q,T]$", enc[["field"]])) {
      field <- enc$field
      nc <- nchar(field)
      enc$type <- ENCODE_MAPPING[[substr(field, nc, nc)]]
      enc$field <- substr(field,1, nc - 2)
    } else if (!hasName(enc,"type") && hasName(enc,"field")) {
      dat <- .get_inline_data(spec)
      if (!is.null(dat)) {
        col <- dat[[enc$field]]
        if (is.factor(col)) {
          enc$type <- "ordinal"          
        } else if (inherits(col, c("POSIXt", "POSIXct", "POSIXlt", "Date"))) {
          enc$type <- "temporal"
        } else if (is.numeric(col)) {
          enc$type <- "quantitative"
        } else{
          enc$type <- "nominal"
        }
      }
    }
    
    if (!hasName(spec,"encoding")) spec$encoding <- list()
    spec[["encoding"]][[.enc]] <- enc
    spec
  }
  
  modify_inner_spec(spec, fn)
  
}

.add_param_to_encoding <- function(spec, .enc, param, value){
  
  fn <- function(spec) {
    if (!hasName(spec,"encoding") || !hasName(spec[["encoding"]], .enc)) {
      stop("Error in adding ", param, " to ", .enc, 
           "\nCould not find ",.enc," encoding in spec.",
           "\nAdd encoding first before adding, ", param, ".")
    }
    
    spec[["encoding"]][[.enc]][[param]] <- value
    spec
  }
  
  modify_inner_spec(spec, fn)
  
}

.add_sort_to_encoding <- function(spec, .enc, value){
  
  .add_param_to_encoding(spec, .enc, "sort", value)
}

.add_sort_obj_to_encoding <- function(spec, .enc, ...) {
  
  .add_sort_to_encoding(spec, .enc, list(...))
 
}

.add_axis_to_encoding <- function(spec, .enc, ...){
  axis_params <- list(...)
  if (hasName(axis_params,"remove") && axis_params[["remove"]]) {
    .add_param_to_encoding(spec, .enc, "axis", NA)
  } else {
    axis_params[["remove"]] <- NULL
    .add_param_to_encoding(spec, .enc, "axis", axis_params)
  }
}

.add_scale_to_encoding <- function(spec, .enc, ...){
  .add_param_to_encoding(spec, .enc, "scale", list(...))
}

.add_legend_to_encoding <- function(spec, .enc, ...){
  .add_param_to_encoding(spec, .enc, "legend", list(...))
}

.add_condition_to_encoding <- function(spec, .enc, ...) {
  
  # adds to an array, so not use standard func...
  
  fn <- function(spec) {
  
    if (!hasName(spec,"encoding") || !hasName(spec[["encoding"]], .enc)) {
      stop("Error in adding condition to ", .enc, 
           "\nCould not find ",.enc," encoding in spec.",
           "\nAdd encoding first before adding condition.")
    }
    
    if (hasName(spec[["encoding"]][[.enc]],"condition")) {
      # Check if named
      if (is.null(names(spec[["encoding"]][[.enc]][["condition"]]))) {
        value <- c(spec[["encoding"]][[.enc]][["condition"]], list(list(...)))
      } else {
        value <- c(list(spec[["encoding"]][[.enc]][["condition"]]), list(list(...)))
      }
    } else {
      value <- list(...)
    }
    
    spec[["encoding"]][[.enc]][["condition"]] <- value
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
vl_encode <- function(spec, ...){
  inputs <- list(...)
  for (n in names(inputs)){
    args <- c(list(spec = spec, .enc = n), inputs[[n]])
    spec <- rlang::exec(.add_encoding, !!!args)
  }
  spec
}
