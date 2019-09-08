.add_transform <- function(spec, obj, reference, ...){
  
  if (!hasName(spec,"transform")) spec$transform <- list()
  validate_sub_schema(obj, reference)
  spec[["transform"]] <- c(spec[["transform"]],list(obj))
  spec

}

.add_bin_to_encoding <- function(spec, obj, ref, encoding, ...) {
  .add_param_to_encoding(spec, obj, ref, encoding, param = "bin", ...)
}

.add_stack_to_encoding <- function(spec, obj, ref, encoding, ...) {
  .add_param_to_encoding(spec, obj, ref, encoding, param = "stack", ...)
}

.add_impute_to_encoding <- function(spec, obj, ref, encoding, ...) {
  .add_param_to_encoding(spec, obj, ref, encoding, param = "impute", ...)
}

.add_aggregate_to_encoding <- function(spec, obj, ref, encoding, ...) {
  .add_param_to_encoding(spec, obj, ref, encoding, param = "aggregate", ...)
}
