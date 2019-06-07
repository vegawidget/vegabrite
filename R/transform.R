.add_transform <- function(spec, .trans, ...){
  
  if (!hasName(spec,"transform")) spec$transform <- list()
  spec[["transform"]] <- c(spec[["transform"]],list(list(...)))
  spec

}

.add_bin_to_encoding <- function(spec, .enc, ...) {
  
  bin_params <- list(...)
  if (length(bin_params) == 0){ bin_params = TRUE}
  .add_param_to_encoding(spec, .enc, "bin", bin_params)
}


.add_stack_to_encoding <- function(spec, .enc, stack) {
  .add_param_to_encoding(spec, .enc, "stack", stack)
}

.add_impute_to_encoding <- function(spec, .enc, ...) {
  impute_params <- list(...)
  .add_param_to_encoding(spec, .enc, "impute", impute_params)
}

.add_aggregate_to_encoding <- function(spec, .enc, op) {
  .add_param_to_encoding(spec, .enc, "aggregate", op)
}