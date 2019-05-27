#' vl_chart
#'
#' @param description Description field for chart
#'
#' @return A vega-lite spec, as an S3 object of class vegaspec using [vegawidget::as_vegaspec()]
#' @export
vl_chart <- function(description = NULL) {
  spec <- list(
    `$schema` = vegawidget::vega_schema()
  )
  if (!is.null(description)) spec$description <- description
  vegawidget::as_vegaspec(spec)
}
