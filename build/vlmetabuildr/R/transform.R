create_transform_functions <- function(schema) {
  transform_options <- purrr::map_chr(schema$definitions$Transform$anyOf, get_name_from_ref)
  c(
    purrr::map_chr(transform_options, create_transform, schema = schema),
    purrr::map_chr(transform_options, create_object, schema = schema),
    purrr::map_chr(transform_options, create_deprecated_object)
  )
}



create_transform <- function(trans, schema) {
  
  short_trans <- tolower(stringr::str_remove(trans,"Transform"))

  make_function( glue("#/definitions/{trans}"), 
                 schema, 
                 short_trans, 
                 ".add_transform", 
                 priority_args = short_trans,
                 pass_to_adder = list(.trans = short_trans),
                 description = glue("Add {trans} to a vega-lite spec."))

}



