make_function_helper <- function(suffix, docs, inner_function, args) {

  ## Make the outer function
  fn <- glue("vl_{suffix} <- function({args}) {{\n{inner_function}\n}}")

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
                          doc_group = NULL,
                          sub_references = NULL) {

  # Make the documentation
  if (is.null(doc_group)) {
    docs <- make_docs(reference, schema, suffix,
      exclude_args = names(override_args),
      description = description, 
      sub_references = sub_references
    )
  } else {
    docs <- make_docs_for_group(doc_group)
  }

  ## Make the inner function
  inner_fn <- make_function_innards(reference, schema, override_args, adder_function, pass_to_adder)

  ## Get args
  args <- make_arg_list(reference, schema, names(override_args), priority_args, sub_references = sub_references )

  ## Assemble
  make_function_helper(suffix, docs, inner_fn, args)
}

make_function_innards <- function(reference, schema, override_args, adder_function, pass_to_adder) {
  modifier <- glue("  obj <- as.list(environment())\n  obj <- .make_object(obj, {deparse_c(override_args)}, NULL)")

  extras <- if (!is.null(pass_to_adder)) {
    paste(c(" ", paste(names(pass_to_adder), purrr::map_chr(pass_to_adder, deparse_c), sep = " = ")),
      collapse = ", "
    )
  } else {
    ""
  }

  adder <- glue("{adder_function}(spec, obj, {deparse_c(as.character(reference))}{extras})")

  paste(
    modifier,
    adder,
    sep = "\n  "
  )
}

make_arg_list <- function(reference, schema, exclude_args, priority_args, sub_references = NULL) {
  param_names <- get_params(schema, reference, exclude_args)
  
  if (!is.null(sub_references)) {
    param_names <- c(param_names, unlist(purrr::map(sub_references, ~get_params(schema, ., exclude = exclude_args))))
  }

  # If param is named repeat, need to change
  param_names[param_names == "repeat"] <- "`repeat`"

  required <- get_required_params(schema, reference)

  param_names <- unique(c(intersect(priority_args, param_names), intersect(required, param_names), param_names))
  args <- paste(param_names, "NULL", sep = " = ")
  arg_list <- paste(c("spec", args, ".object = NULL"), collapse = ", ")

  if (additional_properties_allowed(reference, schema)) {
    arg_list <- paste0(arg_list, ", ...")
  }

  arg_list
}

make_docs_helper <- function(title, description, param_docs,
                             returns = "A modified Vega-Lite Spec", export = TRUE, doc_group = NULL, extra = NULL) {
  title <- roxy_wrap(title)

  desc <- roxy_wrap(description)

  returns <- glue("#' @return {returns}")
  export <- if (export) "#' @export" else NULL

  doc_name <- if (!is.null(doc_group)) glue("#' @name {doc_group}") else NULL

  paste(
    c(
      "",
      title,
      "#' ",
      desc,
      param_docs,
      returns,
      export,
      doc_name,
      extra
    ),
    collapse = "\n"
  )
}

make_docs <- function(reference, schema, suffix, exclude_args, description = "", sub_references = NULL) {
  spec_doc <- glue("#' @param spec An input vega-lite spec")
  param_docs <- get_param_docs(schema, reference, exclude = exclude_args)
  if (!is.null(sub_references)) {
    param_docs_extra <- paste(purrr::map_chr(sub_references, ~get_param_docs(schema, ., exclude = exclude_args)), sep = "\n")
  } else { 
    param_docs_extra <- ""
  }
  object_doc <- get_object_doc(schema, reference)

  make_docs_helper(
    glue("vl_{suffix}"),
    description,
    paste(
      spec_doc, 
      param_docs, 
      param_docs_extra, 
      object_doc,
      sep = "\n")
  )
}

make_docs_for_group <- function(doc_group) {
  glue("\n#' @name {doc_group}\n#' @export", .trim = FALSE)
}


make_group_doc <- function(reference, schema, doc_group, title, description, exclude_args = NULL) {
  spec_doc <- glue("#' @param spec An input vega-lite spec")
  param_docs <- get_param_docs(schema, reference, exclude = exclude_args)
  object_doc <- get_object_doc(schema, reference)

  paste(make_docs_helper(
    title,
    description,
    paste(
        spec_doc, 
        param_docs, 
        object_doc, 
        sep = "\n"
    ),
    doc_group = doc_group,
    export = FALSE,
  ), "NULL", "#> NULL", sep = "\n")
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
                                 doc_group = NULL) {


  # Make the documentation
  if (is.null(doc_group)) {
    docs <- make_option_docs(option_name, options, suffix,
      description = description,
      na_option = na_option
    )
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

opts_to_list <- function(options, na_option) {
  if (na_option) {
    paste(c(paste("'", options, "'", sep = ""), NA), collapse = ", ")
  } else {
    paste("'", options, "'", sep = "", collapse = ", ")
  }
}

make_option_docs <- function(option_name, options, suffix, description, na_option) {
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

  paste(make_docs_helper(
    title,
    description,
    paste(spec_doc, param_docs, sep = "\n"),
    doc_group = doc_group,
    export = FALSE,
  ), "NULL", "#> NULL", sep = "\n")
}

make_option_function_innards <- function(reference, option_name, adder_function, pass_to_adder) {
  matcher <- glue("  {option_name} <- match.arg({option_name})")

  extras <- if (!is.null(pass_to_adder)) {
    paste(c(" ", paste(names(pass_to_adder), purrr::map_chr(pass_to_adder, deparse_c), sep = " = ")),
      collapse = ", "
    )
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

create_deprecated <- function(old, new) {
  glue(
    "#' @export",
    "#' @name vlbuildr-deprecated",
    "{old} <- function(...) {{",
    "  .Deprecated('{new}', package = 'vlbuidlr')",
    "  {new}(...)",
    "}}",
    .sep = "\n", .trim = FALSE
  )
}
