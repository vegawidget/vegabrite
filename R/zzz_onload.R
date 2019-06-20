env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  env$v8 <- V8::v8()
  env$v8$source(system.file("js/ajv.min.js", package = "vlbuildr"))
  schema_file <- Sys.glob(file.path(system.file("schema", package= "vlbuildr"),
                                    "*.json"))
  schema <- readr::read_file(schema_file)
  env$v8$eval(
         sprintf("ajv = new Ajv({verbose: true}).addMetaSchema(AjvSchema6).addSchema(%s,'vega-lite')",
                 schema)
  )
}