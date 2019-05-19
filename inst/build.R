library("vlbuildr")

schema_file <- system.file("schema.json", package = "vlapir")
VL_SCHEMA <- jsonlite::read_json(schema_file)

encoding_options <- props(VL_SCHEMA, list("$ref" = "#/definitions/Encoding"))

mark_options = enums(VL_SCHEMA, list("$ref" = '#/definitions/AnyMark'));

data_funcs <- create_data_generic(VL_SCHEMA)

mark_funcs <- c(
  create_mark_generic(VL_SCHEMA),
  purrr::map_chr(mark_options, create_mark, schema = VL_SCHEMA)
)

encoding_funcs <- purrr::map_chr(names(encoding_options), create_encoder, schema = VL_SCHEMA)

r_api <- c(encoding_funcs, mark_funcs, data_funcs)
r_file_path <- file.path(rprojroot::find_package_root_file(), "R","zzz_autogen_api.R")
cat(r_api, file = r_file_path)
