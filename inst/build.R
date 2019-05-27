library("vlmetabuildr")

schema_file <- Sys.glob(file.path(system.file("schema/vega-lite", package = "vegawidget"),"*.json"))
VL_SCHEMA <- jsonlite::read_json(schema_file)

r_api <- create_api(VL_SCHEMA)

r_file_path <- file.path(rprojroot::find_package_root_file(), "R","zzz_autogen_api.R")
cat(r_api, file = r_file_path)
