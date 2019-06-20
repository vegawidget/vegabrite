
validate_sub_schema <- function(x, obj) {
  name <- basename(tempfile("ajv_"))
  ref <- paste0("#/definitions/", obj)
  env$v8$eval(sprintf("%s = ajv.getSchema('%s')", name, ref))
  json <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
  res <- env$v8$call(name, V8::JS(json))
  errors <- env$v8$get(paste0(name, ".errors"))
  # TODO: Figure out how to format the errors more nicely 
  return(list(result = res, errors = errors))
}
