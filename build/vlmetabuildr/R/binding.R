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
  
  extra_doc <- paste(
    "#' @param parameter_name Name of selection to add binding to",
    "#' @param projection_name Name of projection (field or encoding) within selection",
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
  modifier <- glue("  obj <- as.list(environment())\n  obj <- .make_object(obj, list(input = '{name}'), c('projection_name', 'parameter_name'))")

  adder <- glue(".add_binding(spec, obj, '{reference}', parameter_name = parameter_name,
                projection_name = projection_name)")

  inner_fn <- paste(
    modifier,
    adder,
    sep = "\n  "
  )

  ## Get args
  args <- paste(
    c(
      "projection_name", 
      get_params(schema, reference, exclude = "input")
    ), 
    "NULL", 
    sep = " = ")
  arg_list <- paste(c("spec", "parameter_name", args), collapse = ", ")

  make_function_helper(suffix, docs, inner_fn, arg_list)
}
