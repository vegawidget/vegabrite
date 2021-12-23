create_facet_functions <- function(schema) {
  c(
    create_facet_type(schema, "row"),
    create_facet_type(schema, "column"),
    create_facet_encoding(schema, "row"),
    create_facet_encoding(schema, "column"),
    create_facet_encoding(schema, "facet"),
    create_facet_wrap(schema)
  )
}

create_facet_type <- function(schema, type) {
  make_function(glue("#/definitions/FacetFieldDef"),
    schema,
    glue("facet_{type}"),
    glue(".add_facet_{type}"),
    priority_args = c("field", "type"),
    description = glue::glue("Add faceting by {type} to a vega-lite spec.")
  )
}

create_facet_encoding <- function(schema, type) {
  make_function(glue("#/definitions/FacetEncodingFieldDef"),
    schema,
    glue("encode_{type}"),
    glue(".add_encoding"),
    priority_args = c("field", "type"),
    pass_to_adder = list(encoding = type),
    description = glue::glue("Add faceting by {type} to a vega-lite spec.")
  )
}


create_facet_wrap <- function(schema) {
  reference <- glue("#/definitions/FacetFieldDef")
  suffix <- "facet"

  spec_doc <- glue("#' @param spec An input vega-lite spec")
  object_doc <- get_object_doc(schema, reference)
  extra_doc <- "#' @param columns number of columns to add"
  param_docs <- get_param_docs(schema, reference)

  docs <- make_docs_helper(
    glue("vl_{suffix}"),
    glue::glue("Add wrapped facetting to a vega-lite spec."),
    paste(
      spec_doc, 
      param_docs, 
      extra_doc,
      object_doc,
      sep = "\n")
  )

  ## Make the inner function
  param_names <- get_params(schema, reference)
  modifier <- "  obj <- .make_object(NULL, \"columns\")"

  adder <- glue(".add_facet_wrap(spec, obj, \"{reference}\", columns = columns)")

  inner_fn <- paste(
    modifier,
    adder,
    sep = "\n  "
  )

  ## Get args
  param_names <- unique(c(c("field","type"), param_names))

  args <- paste(param_names, "NULL", sep = " = ")
  arg_list <- paste(
    c("spec", 
      args,
      "columns = 2",
      ".object = NULL"), 
    collapse = ", ")

  ## Make the outer function
  fn <- glue("vl_{suffix} <- function({arg_list}){{\n{inner_fn}\n}}")

  # Combine docs and function
  glue_collapse(c(docs, fn), sep = "\n", last = "\n")
}
