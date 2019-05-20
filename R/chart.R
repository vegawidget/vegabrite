#' vl_chart
#'
#' @param description
#'
#' @return
#' @export
#'
#' @examples
vl_chart <- function(description = NULL) {
  spec <- list(
    `$schema` = vegawidget::vega_schema()
  )
  if (!is.null(description)) spec$description <- description
  vegawidget::as_vegaspec(spec)
}
