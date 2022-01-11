devtools::install("build/vlmetabuildr")
library("vlmetabuildr")

schema_file <- vega_schema("vega_lite", major = FALSE) 
VL_SCHEMA <- jsonlite::read_json(schema_file)

new_schema_path <- file.path(rprojroot::find_package_root_file(), "inst","schema", basename(schema_file))
mini_json = jsonlite:::minify(readr::read_file(schema_file))
cat(mini_json, file = new_schema_path)

r_api <- create_api(VL_SCHEMA)

r_file_path <- file.path(rprojroot::find_package_root_file(), "R","zzz_autogen_api.R")
cat(r_api, file = r_file_path, sep = "\n")
devtools::document()
devtools::install()
