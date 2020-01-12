create_encoding_functions <- function(schema) {
  
  encoding_options <- props("#/definitions/Encoding", schema)
  
  c(
    purrr::map_chr(names(encoding_options), create_encoder, schema = schema),
    purrr::map_chr(names(encoding_options), create_encode_object, schema = schema)
  )
  
}

create_encoder <- function(enc, schema) {

  
  make_function( glue("#/definitions/Encoding/properties/{enc}"), 
                 schema, 
                 glue::glue("encode_{enc}"), 
                 ".add_encoding", 
                 priority_args = c("field","type"),
                 description = glue::glue("Add encoding for {enc} to a vega-lite spec."),
                 pass_to_adder = list(encoding = enc))
}


create_encode_object <- function(enc, schema) {

  Enc <- capitalize(enc)
  create_object(Enc, schema, reference = glue("#/definitions/Encoding/properties/{enc}"))

}

