create_config_functions <- function(schema) {
  top_config_props <- props(glue("#/definitions/Config"), schema)
  is_config <- purrr::map_lgl(top_config_props, is_config_option)
  config_options <- names(top_config_props)[is_config]
  config_objs <- stringr::str_subset(names(schema$definitions), "Config$")
  c(
    create_config(schema),
    purrr::map_chr(config_options, create_sub_config, schema = schema),
    purrr::map_chr(config_objs, create_object, schema = schema)
  )
}

is_config_option <- function(prop, schema) {
  ref_name <- get_name_from_ref(prop)
  if (is.null(ref_name)) {
    return(FALSE)
  }

  stringr::str_detect(ref_name, "Config$")
}

create_sub_config <- function(prop, schema) {
  top_config_props <- props("#/definitions/Config", schema)
  config <- get_name_from_ref(top_config_props[[prop]])

  make_function(glue("#/definitions/{config}"),
    schema,
    glue("config_{prop}"),
    ".add_sub_config",
    description = glue::glue("Add {prop} config ({config}) to a vega-lite spec."),
    pass_to_adder = list(.config = prop)
  )
}

create_config <- function(schema) {
  make_function(glue("#/definitions/Config"),
    schema,
    "config",
    ".add_config",
    description = glue::glue("Add top-level config to a vega-lite spec.")
  )
}