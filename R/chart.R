.add_properties <- function(spec, ...) {
  
  spec <- utils::modifyList(spec, list(...))
  
  return(vegawidget::as_vegaspec(spec))
}
