create_binding_functions <- function(schema) {
  c(
    create_binding(schema, "radio", "BindRadioSelect"),
    create_binding(schema, "select", "BindRadioSelect"),
    create_binding(schema, "checkbox", "BindCheckbox"),
    create_binding(schema, "range", "BindRange")
  )
}

create_binding <- function(schema, name, ref) {
  reference <- glue("#/definitions/{ref}")
  suffix <- glue::glue("bind_{name}_input")

  spec_doc <- glue("#' @param spec An input vega-lite spec")
  object_doc <- get_object_doc(schema, reference)
  extra_doc <- paste(
    "#' @param selection_name Name of selection to add binding to",
    "#' @param projection_name Name of projection (field or encoding) within selection",
    object_doc,
    sep = "\n"
  )
  param_docs <- get_param_docs(schema, reference, exclude = "input")

  docs <- make_docs_helper(
    glue("vl_{suffix}"),
    glue::glue("Add {name} binding to a vega-lite spec."),
    paste(spec_doc, extra_doc, param_docs, sep = "\n")
  )

  ## Make the inner function
  param_names <- get_params(schema, reference)
  modifier <- glue("  obj <- .modify_args(list(input = '{name}'), c('projection_name', 'selection_name'))")

  adder <- glue(".add_binding(spec, obj, '{reference}', selection_name = selection_name,
                projection_name = projection_name)")

  inner_fn <- paste(
    modifier,
    adder,
    sep = "\n  "
  )

  ## Get args
  args <- paste(c("projection_name", ".object", get_params(schema, reference, exclude = "input")), "NULL", sep = " = ")
  arg_list <- paste(c("spec", "selection_name", args), collapse = ", ")

  make_function_helper(suffix, docs, inner_fn, arg_list)
}
