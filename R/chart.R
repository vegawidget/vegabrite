#' vl_chart
#'
#' Initialize a Vega-Lite specification! Can add any top level configuration
#' parameters, or simply call without arguments to initialize and then use other
#' function (like [vl_mark_point()], [vl_encode_x()], etc) to add on the various
#' pieces of the chart spec.
#'
#' @return A vega-lite spec, as an S3 object of class vegaspec_vega_lite
#'  using [vegawidget::as_vegaspec()]
#' @export
#' @name vl_chart
#' @importFrom utils hasName
#' @importFrom vegawidget as_vegaspec
#' @examples
#'
#' vl_chart() %>%
#'   vl_add_data(values = mtcars) %>%
#'   vl_mark_point() %>%
#'   vl_encode_x("wt") %>%
#'   vl_encode_y("mpg")
NULL

#' vl_add_data_frame
#'
#' Add data in the format of an R data frame
#'
#' @param spec An input vega-lite spec
#' @param values Data frame with data to add to chart
#' @param ... Additional arguments to pass to [vl_add_data()], such as `name` or `format`
#' @return A modified Vega-Lite Spec
#' @export
#' @seealso [vl_add_data()]
#' @examples
#'
#' vl_chart() %>%
#'   vl_add_data_frame(mtcars) %>%
#'   vl_mark_point() %>%
#'   vl_encode_x("wt") %>%
#'   vl_encode_y("mpg")
vl_add_data_frame <- function(spec, values, ...) {
  vl_add_data(spec, values = values, ...)
}

#' vl_add_data_url
#'
#' Add data in the form of a url
#'
#' @param spec An input vega-lite spec
#' @param url Data frame with data to add to chart
#' @param ... Additional arguments to pass to [vl_add_data()], such as `name` or `format`
#' @return A modified Vega-Lite Spec
#' @export
#' @seealso [vl_add_data()]
#' @examples
#'
#' vl_chart() %>%
#'   vl_add_data_url("https://vega.github.io/vega-editor/app/data/movies.json") %>%
#'   vl_encode_x(field = "IMDB_Rating", type = "quantitative") %>%
#'   vl_encode_y(type = "quantitative", aggregate = "count") %>%
#'   vl_bin_x(maxbins = 10) %>%
#'   vl_mark_bar()
vl_add_data_url <- function(spec, url, ...) {
  vl_add_data(spec, url = url, ...)
}

.chart_initialize <- function(obj) {
  if (hasName(obj, "data")) {
    obj$data <- .data_infer(obj$data)
  }
  as_vegaspec(obj)
}

.add_properties <- function(spec, object, ref) {
  spec <- utils::modifyList(spec, object)
  vegawidget::as_vegaspec(spec)
}

.add_data <- function(spec, obj, ref, ...) {
  obj <- .data_infer(obj)
  .add_to_top_spec(spec, obj, name = "data", ref = ref, how = "replace")
}

.data_infer <- function(obj) {
  if (is.data.frame(obj)) {
    obj <- list(values = obj)
  } else if (is.character(obj)) {
    obj <- list(url = obj)
  }
  obj
}
