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


.add_properties <- function(spec, ...) {
  
  spec <- utils::modifyList(spec, list(...))
  
  return(vegawidget::as_vegaspec(spec))

}

.add_data <- function(spec, obj, ref, ...) {
  
  .add_to_top_spec(spec, obj, name = 'data', ref = ref, how = 'replace')

}