env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  schema_file <- Sys.glob(file.path(system.file("schema", package = "vlr"), "*.json"))
  env$VL_SCHEMA <- jsonlite::read_json(schema_file)
  lockEnvironment(vl, bindings = TRUE)
}
