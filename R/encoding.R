.add_encoding <- function(spec, .enc, ...){

  if (is_composite(spec)) {
    stop("Can't add encoding to composite spec; add encoding prior to combining specs")
  }
  if (hasName(spec, "spec")) {
    inner_spec <- TRUE
    outer_spec <- spec
    spec <- outer_spec[["spec"]]
  } else {
    inner_spec <- FALSE
  }

  if (!hasName(spec,"encoding")) spec$encoding <- list()
  spec[["encoding"]][[.enc]] <- list(...)

  if (inner_spec) {
    outer_spec[["spec"]] <- spec
    spec <- outer_spec
  }
  return(spec)
}


#' vl_encoding
#'
#' Add encodings to a spec. This is one way to add encodings... each encoding
#' can also be added using vl_encoding_<name> where <name> is the name of the
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
#'     x = vl_X(field = "wt", type = "quantitative"),
#'     y = vl_Y(field = "mpg", type = "quantitative")
#'   )
vl_encode <- function(spec, ...){
  inputs <- list(...)
  for (n in names(inputs)){
    args <- c(list(spec = spec, .enc = n), inputs[[n]])
    spec <- rlang::exec(.add_encoding, !!!args)
  }
  spec
}
