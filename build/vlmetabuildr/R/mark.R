create_mark <- function(mark, schema) {

  make_function( "#/definitions/MarkDef", 
                 schema, 
                 glue::glue("mark_{mark}"), 
                 ".add_mark", 
                 override_args = list(type = mark))

}

# Helper function to get the reference for a composite definition
get_composite_def <- function(schema, comp){
  def_names <- stringr::str_subset(names(schema$definitions),"Def$")
  comp_def <- def_names[stringr::str_replace(tolower(def_names),"def$","") == comp]
  
  glue::glue("#/definitions/{comp_def}")
}

create_mark_composite <- function(mark, schema) {
  
  
  make_function( get_composite_def(schema, mark), 
                 schema, 
                 glue::glue("mark_{mark}"), 
                 ".add_mark", 
                 override_args = list(type = mark))
  
}

create_mark_functions <- function(schema){
  simple_mark_options = enums(schema, list("$ref" = '#/definitions/Mark'))
  comp_mark_options = enums(schema, list("$ref" = '#/definitions/CompositeMark'))
  c(
    purrr::map_chr(simple_mark_options, create_mark, schema = schema),
    purrr::map_chr(comp_mark_options, create_mark_composite, schema = schema)
  )
}