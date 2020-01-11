make_function_helper <- function(suffix, docs, inner_function, args) {
  
  ## Make the outer function
  fn <- glue("vl_{suffix} <- function({args}){{\n{inner_function}\n}}")
  
  # Combine docs and function
  glue_collapse(c(docs, fn), sep = "\n", last = "\n")
  
}


make_function <- function(
  reference,
  schema,
  suffix,
  adder_function,
  description = "",
  override_args = NULL,
  priority_args = NULL,
  pass_to_adder = NULL,
  doc_group = NULL
) {
  
  # Make the documentation
  if (is.null(doc_group)) {
    docs <- make_docs(reference, schema, suffix,  exclude_args = names(override_args), 
                      description = description)
  } else {
    docs <- make_docs_for_group(doc_group)
  }
  
  ## Make the inner function
  inner_fn <- make_function_innards(reference, schema, override_args, adder_function, pass_to_adder)
  
  ## Get args
  args <- make_arg_list(reference, schema, names(override_args), priority_args)
  
  ## Assemble
  make_function_helper(suffix, docs, inner_fn, args)
  
}

make_function_innards <- function(reference, schema, override_args, adder_function, pass_to_adder) {
  
  param_names <- get_params(schema, reference)
  modifier <- glue("  args <- .modify_args({deparse_c(override_args)}, {deparse_c(param_names)})")
  
  extras <- if (!is.null(pass_to_adder)){
    paste(c(" ",paste(names(pass_to_adder), purrr::map_chr(pass_to_adder, deparse_c), sep = " = ")), 
          collapse = ", ")
  } else {
    ""
  }
  
  adder <- glue("{adder_function}(args$spec, args$object, '{reference}'{extras})")
  
  paste(
    modifier,
    adder, 
    sep = "\n  "
  )
  
}

make_arg_list <- function(reference, schema, exclude_args, priority_args){
  
  param_names <- get_params(schema, reference, exclude_args)
  
  # If param is named repeat, need to change
  param_names[param_names == "repeat"] <- "`repeat`"
  
  param_names <- unique(c(intersect(priority_args,param_names), param_names))
  args <- paste(param_names, "NULL", sep = " = ")
  arg_list <- paste(c('spec', args), collapse = ", ")
  
  arg_list
}

make_docs_helper <- function(title, description, param_docs,
                             returns = "A modified Vega-Lite Spec", export = TRUE, doc_group = NULL) {
  
  title <- roxy_wrap(title)
  
  desc <- roxy_wrap(description)
  
  returns <- glue("#' @return {returns}")
  export <- if (export) "#' @export" else NULL
  
  doc_name <- if (!is.null(doc_group)) glue("#' @name {doc_group}") else NULL
  
  paste(
    "",
    title,
    "#' ",
    desc,
    param_docs,
    returns,
    export,
    doc_name,
    sep = "\n"
  )
  
}

make_docs <- function(reference, schema, suffix,  exclude_args,  description = "") {
  
  spec_doc <- glue("#' @param spec An input vega-lite spec")
  param_docs <- get_param_docs(schema, reference, exclude = exclude_args)
  
  make_docs_helper(
    glue("vl_{suffix}"),
    description,
    paste(spec_doc, param_docs, sep = "\n")
  )
}

make_docs_for_group <- function(doc_group){
  glue("\n#' @name {doc_group}\n#' @export", .trim = FALSE)
}


make_group_doc <- function(reference, schema, doc_group, title, description, exclude_args = NULL) {
  
  spec_doc <- glue("#' @param spec An input vega-lite spec")
  param_docs <- get_param_docs(schema, reference, exclude = exclude_args)
  
  make_docs_helper(
    title,
    description,
    paste(spec_doc, param_docs, sep = "\n"),
    doc_group = doc_group
  )
  
}



make_option_function <- function(
  reference,
  option_name,
  options,
  suffix,
  adder_function,
  na_option = FALSE,
  description = "",
  pass_to_adder = NULL,
  doc_group = NULL
) {
  
  
  # Make the documentation
  if (is.null(doc_group)) {
    docs <- make_option_docs(option_name, options, suffix, description = description, 
                             na_option = na_option)
  } else {
    docs <- make_docs_for_group(doc_group)
  }
  
  ## Make the inner function
  inner_fn <- make_option_function_innards(reference, option_name, adder_function, pass_to_adder)
  
  ## Get args
  args <- make_option_arg_list(option_name, options, na_option)
  
  ## Assemble
  make_function_helper(suffix, docs, inner_fn, args)
}

opts_to_list <- function(options, na_option){
  if (na_option){
    paste(c(paste("'",options,"'", sep = ""),NA), collapse = ", ")
  } else {
    paste("'",options,"'", sep = "", collapse = ", ")
  }
}

make_option_docs<- function(option_name, options, suffix, description, na_option) {
  
  spec_doc <- glue("#' @param spec An input vega-lite spec")
  param_docs <- glue("#' @param {option_name} One of {opts_to_list(options, na_option)}")
  
  make_docs_helper(
    glue("vl_{suffix}"),
    description,
    paste(spec_doc, param_docs, sep = "\n")
  )

}

make_option_group_doc <- function(doc_group, option_name, options, title, description, na_option) {
  
  spec_doc <- glue("#' @param spec An input vega-lite spec")
  param_docs <- glue("#' @param {option_name} One of {opts_to_list(options, na_option)}")
  
  make_docs_helper(
    title,
    description,
    paste(spec_doc, param_docs, sep = "\n"),
    doc_group = doc_group
  )
  
}

make_option_function_innards <- function(reference, option_name, adder_function, pass_to_adder) {
  
  matcher <- glue("  {option_name} <- match.arg({option_name})")
  
  extras <- if (!is.null(pass_to_adder)){
     paste(c(" ",paste(names(pass_to_adder), purrr::map_chr(pass_to_adder, deparse_c), sep = " = ")), 
                  collapse = ", ")
  } else {
    ""
  }
  
  adder <- glue("{adder_function}(spec, {option_name}, '{reference}'{extras})")
  
  paste(
    matcher,
    adder, 
    sep = "\n  "
  )
}

make_option_arg_list <- function(option_name, options, na_option = FALSE) {
  glue("spec, {option_name} = c({opts_to_list(options, na_option)})")
}
