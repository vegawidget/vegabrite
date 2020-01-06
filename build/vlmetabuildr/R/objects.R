
create_object <- function(obj, schema, reference = glue("#/definitions/{obj}")) {
  
  suffix <- glue("make_{obj}")
  
  docs <- make_docs_helper(
    glue("vl_make_{obj}"),
    "Create spec for {obj}",
    get_param_docs(schema, reference),
    returns = glue("A component of a Vega-Lite spec, corresponding to a {obj}.")
  )
  
  param_names <- get_params(schema, reference)
  
  # If param is named repeat, need to change
  param_names[param_names == "repeat"] <- "`repeat`"
  
  args <- paste(param_names, "NULL", sep = " = ", collapse = ", ")

  inner_fn <- glue("  args <- .modify_args(NULL, {deparse_c(param_names)})\n  args$obj")
  
  make_function_helper(suffix, docs, inner_fn, args)
  
}


create_additional_objects <- function(schema) {
  
  objs <- c("BinParams",
            "Axis",
            "Scale",
            "Legend",
            "BindCheckbox",
            "BindRange",
            "BindRadioSelect")
  
  purrr::map_chr(objs, create_object, schema = schema)
  
}


