
create_resolve_functions <- function(schema) {
  
  resolve_doc <- make_option_group_doc(
    "resolve",
    "how",
    unlist(enums("#/definitions/ResolveMode", schema)),
    "Resolve axes, legends, or scales for composite charts",
    paste(
      "When faceting, layering, repeating, or concatenating a chart, one ",
      "can choose whether axes, legends, or scales are shared or independent ",
      "using the resolve specification"),
    na_option = FALSE
  )
  
  c(
    resolve_doc,
    create_resolve_axis_functions(schema),
    create_resolve_legend_functions(schema),
    create_resolve_scale_functions(schema)
  )
}

create_resolve <- function(enc, type, schema) {
  
  make_option_function(
    "#/definitions/ResolveMode",
    "how",
    unlist(enums("#/definitions/ResolveMode", schema)),
    glue("resolve_{type}_{enc}"),
    ".add_resolve",
    na_option = FALSE,
    pass_to_adder = list(encoding = enc, type = type),
    doc_group = "resolve"
  )
  
}

create_resolve_axis_functions <- function(schema){
  
  encs <- names(schema$definitions$AxisResolveMap$properties)
  purrr::map_chr(encs, create_resolve, type = "axis", schema = schema)
  
}

create_resolve_scale_functions <- function(schema){
  
  encs <- names(schema$definitions$ScaleResolveMap$properties)
  purrr::map_chr(encs, create_resolve, type = "scale", schema = schema)
  
}

create_resolve_legend_functions <- function(schema){
  
  encs <- names(schema$definitions$ScaleResolveMap$properties)
  purrr::map_chr(encs, create_resolve, type = "legend", schema = schema)
  
}

