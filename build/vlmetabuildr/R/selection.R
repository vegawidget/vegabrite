create_selection_functions <- function(schema) {
  selections <- list("SingleSelection", "MultiSelection", "IntervalSelection")
  c(
    purrr::map_chr(selections, create_selection_type, schema = schema),
    purrr::map_chr(selections, create_object, schema = schema),
    purrr::map_chr(selections, create_deprecated_object)
  )
}


create_selection_type <- function(type, schema) {
  reference <- glue("#/definitions/{type}")
  short_type <- tolower(stringr::str_remove(type, "Selection"))
  suffix <- glue::glue("add_{short_type}_selection")

  spec_doc <- glue("#' @param spec An input vega-lite spec")
  extra_doc <- "#' @param selection_name Name for selection"
  param_docs <- get_param_docs(schema, reference)
  object_doc <- get_object_doc(schema, reference)

  docs <- make_docs_helper(
    glue("vl_{suffix}"),
    glue::glue("Add {type} to a vega-lite spec."),
    paste(spec_doc, object_doc, extra_doc, param_docs, sep = "\n")
  )

  ## Make the inner function
  param_names <- get_params(schema, reference)
  modifier <- "  obj <- .modify_args(NULL, 'selection_name')"

  adder <- glue(".add_selection(spec, obj, '{reference}', type = '{short_type}', selection_name = selection_name)")

  inner_fn <- paste(
    modifier,
    adder,
    sep = "\n  "
  )

  ## Get args
  args <- paste(param_names, "NULL", sep = " = ")
  arg_list <- paste(c("spec", "selection_name", ".object = NULL", args), collapse = ", ")

  make_function_helper(suffix, docs, inner_fn, arg_list)
}
