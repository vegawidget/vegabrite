create_mark_functions <- function(schema) {
  simple_mark_options <- enums("#/definitions/Mark", schema)
  comp_mark_options <- enums("#/definitions/CompositeMark", schema)
  c(
    purrr::map_chr(simple_mark_options, create_mark, schema = schema),
    purrr::map_chr(comp_mark_options, create_mark_composite, schema = schema)
  )
}

create_mark <- function(mark, schema) {
  make_function("#/definitions/MarkDef",
    schema,
    glue::glue("mark_{mark}"),
    ".add_mark",
    override_args = list(type = mark)
  )
}

create_mark_composite <- function(mark, schema) {
  make_function(glue::glue("#/definitions/{mark}Def"),
    schema,
    glue::glue("mark_{tolower(mark)}"),
    ".add_mark",
    override_args = list(type = tolower(mark))
  )
}
