library("vlmetabuildr")
library("here")

dir_schema <- here("inst", "schema")

schema_file <- Sys.glob(file.path(dir_schema,"*.json"))
VL_SCHEMA <- jsonlite::read_json(schema_file)

r_api <- create_api(VL_SCHEMA)

r_file_path <- file.path(rprojroot::find_package_root_file(), "R","zzz_autogen_api.R")
cat(r_api, file = r_file_path)
