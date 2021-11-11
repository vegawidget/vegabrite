create_additional_objects <- function(schema) {
  objs <- c(
    "BinParams",
    "Axis",
    "Scale",
    "Legend",
    "BindCheckbox",
    "BindRange",
    "BindRadioSelect",
    "SortField"
  )

  c(
    purrr::map_chr(objs, create_object, schema = schema),
    create_object("Window", schema, "#/definitions/WindowFieldDef")
  )
}

get_objects <- function(schema) {
  is_obj <- purrr::map_lgl(schema$definitions, ~ hasName(., "type") && .[["type"]] == "object")
  names(schema$definitions)[is_obj]
}

create_object <- function(obj, schema, reference = glue("#/definitions/{obj}")) {
  fn_name <- glue("vl${obj}")

  # Get whether any additional properties...

  param_docs <- get_param_docs(schema, reference)

  docs <- make_docs_helper(
    obj,
    glue("Create object for {obj}"),
    param_docs,
    returns = glue("A component of a Vega-Lite spec, corresponding to {obj} definition."),
    export = FALSE,
    doc_group = fn_name
  )

  param_names <- get_params(schema, reference)

  # If param is named repeat, need to change
  # param_names[param_names == "repeat"] <- "`repeat`"

  additional_args <- if (additional_properties_allowed(reference, schema)) "..." else NULL
  prop_args <- if (length(param_names) > 1) paste(paste0("`", param_names, "`"), "NULL", sep = " = ", collapse = ", ") else NULL

  args <- paste(c(prop_args, additional_args), collapse = ", ")

  inner_function <- glue("  .modify_args(NULL, NULL)")

  ## Make the outer function
  fn <- glue("vl$`{obj}` <- function({args}){{\n{inner_function}\n}}\n")

  # Combine docs and function
  #glue_collapse(c(docs, fn), sep = "\n", last = "\n")
  fn
}
