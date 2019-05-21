library("vlmetabuildr")

schema_file <- Sys.glob(file.path(system.file("schema/vega-lite", package = "vegawidget"),"*.json"))
VL_SCHEMA <- jsonlite::read_json(schema_file)


# Create data function
data_funcs <- create_data_generic(VL_SCHEMA)

## Create Mark Functions
mark_options = enums(VL_SCHEMA, list("$ref" = '#/definitions/AnyMark'));
mark_funcs <- c(
  create_mark_generic(VL_SCHEMA),
  purrr::map_chr(mark_options, create_mark, schema = VL_SCHEMA)
)

## Create encoding functions
encoding_options <- props(VL_SCHEMA, list("$ref" = "#/definitions/Encoding"))

encoding_funcs <- purrr::map_chr(names(encoding_options), create_encoder, schema = VL_SCHEMA)
encoding_objs <- purrr::map_chr(names(encoding_options), create_encode_object, schema = VL_SCHEMA)

## create transform functions
transform_options <- purrr::map_chr(VL_SCHEMA$definitions$Transform$anyOf, get_name_from_ref)
transform_funcs <- purrr::map_chr(transform_options, create_transform, schema = VL_SCHEMA)
transform_objs <- purrr::map_chr(transform_options, create_object, schema = VL_SCHEMA)


r_api <- c(encoding_funcs, encoding_objs, 
           mark_funcs, 
           data_funcs,
           transform_funcs,
           transform_objs)
r_file_path <- file.path(rprojroot::find_package_root_file(), "R","zzz_autogen_api.R")
cat(r_api, file = r_file_path)
