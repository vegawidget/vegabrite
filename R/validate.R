
validate_sub_schema <- function(x, ref) {
  j <-  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
  schema_file <-  Sys.glob(file.path(system.file("schema/", package = "vlbuildr"),"*.json"))
  res <- jsonvalidate::json_validate(j, readr::read_file(schema_file), engine = "ajv", reference = ref)
  if (!res){
    warning("Invalid specification for ", ref, ".", )
  }
  return(res)
}
