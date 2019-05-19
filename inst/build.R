
r_api <- create_encoder(VL_SCHEMA, "color")
r_file_path <- file.path(rprojroot::find_package_root_file(), "R","zzz_autogen_api.R")
cat(r_api, file = r_file_path)
