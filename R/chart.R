.add_properties <- function(spec, ...) {
  
  spec <- utils::modifyList(spec, list(...))
  
  return(vegawidget::as_vegaspec(spec))

}

.add_data <- function(spec, obj, ref, ...) {
  
  .add_to_top_spec(spec, obj, name = 'data', ref = ref, how = 'replace')

}