create_facet_type <- function(schema, type) {

  make_function( glue("#/definitions/FacetFieldDef"), 
                 schema, 
                 glue("facet_{type}"), 
                 glue(".add_facet_{type}"), 
                 priority_args = c("field","type"),
                 description = glue::glue("Add faceting by {type} to a vega-lite spec.")
  )
  
}

create_facet_encoding <- function(schema, type) {
  make_function( glue("#/definitions/FacetEncodingFieldDef"), 
                 schema, 
                 glue("encode_{type}"), 
                 glue(".add_encoding"), 
                 priority_args = c("field","type"),
                 pass_to_adder = list(encoding = type),
                 description = glue::glue("Add faceting by {type} to a vega-lite spec.")
  )
}


create_facet_wrap <- function(schema) {
  
  reference <- glue("#/definitions/FacetFieldDef")
  suffix <- "facet_wrap"
  
  spec_doc <- glue("#' @param spec An input vega-lite spec")
  extra_doc <- "#' @param columns number of columns to add"
  param_docs <- get_param_docs(schema, reference)
  
  docs <- make_docs_helper(
    glue("vl_{suffix}"),
    glue::glue("Add wrap facetting to a vega-lite spec."),
    paste(spec_doc,extra_doc, param_docs, sep = "\n")
  )
  
  ## Make the inner function
  param_names <- get_params(schema, reference)
  modifier <- glue("  args <- .modify_args(NULL, {deparse_c(param_names)})")
  
  adder <- glue(".add_facet_wrap(args$spec, args$object, '{reference}', columns = args$extra$columns)")
  
  inner_fn <- paste(
    modifier,
    adder, 
    sep = "\n  "
  )
  
  ## Get args
  args <- paste(param_names, "NULL", sep = " = ")
  arg_list <- paste(c('spec', 'columns = 2', args), collapse = ", ")
  
  ## Make the outer function
  fn <- glue("vl_{suffix} <- function({arg_list}){{\n{inner_fn}\n}}")
  
  # Combine docs and function
  glue_collapse(c(docs, fn), sep = "\n", last = "\n")
  
}

#' @export
create_facet_functions <- function(schema){
  c(
    create_facet_type(schema, "row"),
    create_facet_type(schema, "column"),
    create_facet_encoding(schema, "row"),
    create_facet_encoding(schema, "column"),
    create_facet_encoding(schema, "wrap"),
    create_facet_wrap(schema)
  )
}