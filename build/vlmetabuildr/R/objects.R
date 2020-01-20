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

get_objects <- function(schema) {
  is_obj <- purrr::map_lgl(schema$definitions, ~hasName(.,"type") && .[["type"]] == "object")
  names(schema$definitions)[is_obj]
}

create_object <- function(obj, schema, reference = glue("#/definitions/{obj}")) {
  
  fn_name <- glue("vl${obj}")
  
  # Get whether any additional properties...
  additional_properties <-  lookup(schema, reference)$additionalProperties 
  
  param_docs <-  get_param_docs(schema, reference)
  if (!identical(additional_properties, FALSE)) {
    param_docs <- paste(param_docs, "#' @param ... Additional objects", sep = "\n")
  }
  
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
  #param_names[param_names == "repeat"] <- "`repeat`"
  
  additional_args <- if (!identical(additional_properties, FALSE)) "..." else NULL
  prop_args <- if (length(param_names) > 1) paste(paste0('`',param_names,'`'), "NULL", sep = " = ", collapse = ", ") else NULL 
  
  args <- paste(c(prop_args, additional_args), collapse = ", ")

  inner_function <- glue("  .modify_args(NULL, NULL)")
  
  ## Make the outer function
  fn <- glue("vl$`{obj}` <- function({args}){{\n{inner_function}\n}}")
  
  # Combine docs and function
  glue_collapse(c(docs, fn), sep = "\n", last = "\n")
  
}





