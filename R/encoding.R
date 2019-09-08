ENCODE_MAPPING <- list(
  "N" = "nominal",
  "T" = "temporal",
  "Q" = "quantitative",
  "O" = "ordinal"
)

.add_encoding <- function(spec, obj, ref, encoding){

  fn <- function(spec) {
    
    # Handle shorthand
    enc <- obj
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
    validate_sub_schema(enc, ref)
    spec[["encoding"]][[encoding]] <- enc
    spec
  }
  
  modify_inner_spec(spec, fn)
  
}

.add_param_to_encoding <- function(spec, obj, ref, encoding, param, ...){
  
  fn <- function(spec) {
    if (!hasName(spec,"encoding") || !hasName(spec[["encoding"]], encoding)) {
      stop("Error in adding ", param, " to ", encoding, 
           "\nCould not find ", encoding," encoding in spec.",
           "\nAdd encoding first before adding, ", param, ".")
    }
    validate_sub_schema(obj, ref)
    spec[["encoding"]][[encoding]][[param]] <- obj
    spec
  }
  
  modify_inner_spec(spec, fn)
  
}

.add_sort_to_encoding <- function(spec, obj, ref, encoding, ...){
  .add_param_to_encoding(spec, obj, ref, encoding, param = "sort", ...)
}


.add_axis_to_encoding <- function(spec, obj, ref, encoding, ...){
  .add_param_to_encoding(spec, obj, ref, encoding, param = "axis", ...)
}

.add_scale_to_encoding <- function(spec, obj, ref, encoding, ...){
  .add_param_to_encoding(spec, obj, ref, encoding, param = "scale", ...)
}

.add_legend_to_encoding <- function(spec, obj, ref, encoding, ...){
  .add_param_to_encoding(spec, obj, ref, encoding, param = "legend", ...)
}

.add_condition_to_encoding <- function(spec, obj, ref, encoding, ...) {
  
  # adds to an array, so not use standard func...
  
  fn <- function(spec) {
  
    if (!hasName(spec,"encoding") || !hasName(spec[["encoding"]], encoding)) {
      stop("Error in adding condition to ", encoding, 
           "\nCould not find ",encoding," encoding in spec.",
           "\nAdd encoding first before adding condition.")
    }
    
    validate_sub_schema(obj, ref)
    
    if (hasName(spec[["encoding"]][[encoding]],"condition")) {
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
vl_encode <- function(spec, ...){
  inputs <- list(...)
  for (n in names(inputs)){
    args <- list(spec = spec, obj = inputs[[n]], ref = paste0("#/definitions/Encoding/properties/",n), encoding = n)
    spec <- rlang::exec(.add_encoding, !!!args)
  }
  spec
}
