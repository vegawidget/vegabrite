create_encoding_functions <- function(schema) {
  encoding_options <- props("#/definitions/Encoding", schema)

  c(
    purrr::map_chr(names(encoding_options), create_encoder, schema = schema),
    create_encoder_array('tooltip',schema),
    purrr::map_chr(names(encoding_options), create_encode_object, schema = schema)
  )
}

create_encoder <- function(enc, schema) {

  make_function(glue::glue("#/definitions/Encoding/properties/{enc}"),
    schema,
    glue::glue("encode_{enc}"),
    ".add_encoding",
    priority_args = c("field", "type"),
    description = glue::glue("Add encoding for {enc} to a vega-lite spec."),
    pass_to_adder = list(encoding = enc)
  )

}

create_encoder_array <- function(enc, schema) {
  # Just for tooltip for now
  reference <- glue::glue("#/definitions/Encoding/properties/{enc}")
  inner_fn <- glue::glue(".add_encoding_array(spec, array, '{reference}', encoding = '{enc}')")
  docs <- paste(
    glue::glue("#' @rdname vl_encode_{enc}"),
    glue::glue("#' @param array Array of inputs for {enc}"),
    glue::glue("#' @export"),
    sep = "\n"
  )
  arg_list <- paste(c("spec", "array"), collapse = ", ")
  
  make_function_helper(
    glue::glue("encode_{enc}_array"), 
    docs, 
    inner_fn, 
    arg_list
  )
  
}

create_encode_object <- function(enc, schema) {
  Enc <- capitalize(enc)
  create_object(Enc, schema, reference = glue("#/definitions/Encoding/properties/{enc}"))
}
