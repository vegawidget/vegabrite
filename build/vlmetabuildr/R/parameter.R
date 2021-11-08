create_parameter_functions <- function(schema) {
  parameters <- list("VariableParameter","SelectionParameter")
  c(
    purrr::map_chr(parameters, create_parameter_type, schema = schema),
    purrr::map_chr(parameters, create_object, schema = schema)
  )
}


create_parameter_type <- function(type, schema) {
  
  reference <- glue("#/definitions/{type}")
  short_type <- tolower(stringr::str_remove(type, "Parameter"))
  suffix <- glue::glue("add_{short_type}_parameter")
  
  make_function(reference,
                schema,
                suffix,
                ".add_parameter",
                "Add a paramter to a spec"
  )
                
}

  