create_data_generic <- function(schema) {

  make_function( "#/definitions/Data", 
                 schema, 
                 "add_data", 
                 ".add_data", 
                 priority_args = c('values', 'format', 'name', 'url'),
                 description = "Add data to a vega-lite spec")

}
