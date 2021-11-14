create_parameter_functions <- function(schema) {
  
  variable_parameter <- make_function(
    "#/definitions/VariableParameter",
    schema,
    'add_parameter',
    ".add_parameter",
    "Add a variable parameter to a spec.  See [vlbuildr::vl_add_point_selection()] and [vlbuildr::vl_add_interval_selection()] for adding selection parameters."
  )
  
  selection_types <-  c("PointSelectionConfig","IntervalSelectionConfig")
  parameters <- list("VariableParameter","SelectionParameter")
  
  c(
    variable_parameter,
    purrr::map_chr(selection_types, create_selection_function, schema = schema),
    purrr::map_chr(parameters, create_object, schema = schema),
    purrr::map_chr(selection_types, create_object, schema = schema)
  )
}


create_selection_function <- function(type, schema) {
  
  sub_reference <- glue("#/definitions/{type}")
  short_type <- tolower(stringr::str_remove(type, "SelectionConfig"))
  suffix <- glue::glue("add_{short_type}_selection")
  
  make_function(
    "#/definitions/SelectionParameter",
    schema,
    suffix,
    ".add_selection",
    glue("Add a parameter for a {short_type} selection to a spec"),
    override_args = list('type' = short_type),
    sub_references = sub_reference
  )
  
}
