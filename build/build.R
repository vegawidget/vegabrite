library("vlmetabuildr")

schema_file <- Sys.glob(file.path(system.file("schema",'vega-lite', package = 'vegawidget'),"*.json"))
VL_SCHEMA <- jsonlite::read_json(schema_file)

new_schema_path <- file.path(rprojroot::find_package_root_file(), "inst","schema", basename(schema_file))
fs::file_copy(schema_file, new_schema_path, overwrite = TRUE)

r_api <- create_api(VL_SCHEMA)

r_file_path <- file.path(rprojroot::find_package_root_file(), "R","zzz_autogen_api.R")
cat(r_api, file = r_file_path)
