
validate_sub_schema <- function(x, ref) {
  j <-  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
  schema_file <-  Sys.glob(file.path(system.file("schema/", package = "vlbuildr"),"*.json"))
  schema <- paste(readLines(schema_file), collapse = "\n")
  res <- jsonvalidate::json_validate(j, schema, engine = "ajv", reference = ref)
  if (!res){
    warning("Invalid specification for ", ref, ".")
  }
  return(res)
}
