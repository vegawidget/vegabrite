create_encoding_param_functions <- function(schema) {
  c(
    create_function_group_for_encode_param("#/definitions/BinParams", "bin", schema),
    create_function_group_for_encode_param("#/definitions/ImputeParams", "impute", schema),
    create_function_group_for_encode_param("#/definitions/Axis", "axis", schema),
    create_function_group_for_encode_param("#/definitions/Scale", "scale", schema),
    create_function_group_for_encode_param("#/definitions/Legend", "legend", schema),
    create_condition_encoding_functions(schema),
    create_stack_encoding_functions(schema),
    create_aggregate_encoding_functions(schema),
    create_sort_encoding_functions(schema),
    create_sort_by_field_functions(schema),
    create_sort_by_encoding_functions(schema),
    create_remove_axis_functions(schema)
  )
}

get_enc_with_prop <- function(schema, prop) {
  encs <- names(props("#/definitions/Encoding", schema))
  with_prop <-
    purrr::map_lgl(
      encs,
      function(x) {
        prop_names <- names(props(glue("#/definitions/Encoding/properties/{x}"), schema))
        prop %in% prop_names
      }
    )
  encs[with_prop]
}

create_function_for_encode_param <- function(enc, param, reference, schema) {
  make_function(reference,
    schema,
    glue("{param}_{enc}"),
    glue(".add_{param}_to_encoding"),
    doc_group = glue("{param}_encoding"),
    pass_to_adder = list(encoding = enc)
  )
}



create_function_group_for_encode_param <- function(reference, param, schema) {
  doc <- make_group_doc(reference, schema,
    doc_group = glue("{param}_encoding"),
    title = glue("Add {param} to encoding"),
    description = glue("Add {param} parameters to an encoding")
  )

  c(
    doc,
    purrr::map_chr(get_enc_with_prop(schema, param),
      create_function_for_encode_param,
      schema = schema, param = param, reference = reference
    )
  )
}

create_remove_axis_function <- function(enc, schema) {
  docs <- "\n#' @name axis_encoding\n#' @export"

  ## Make the inner function

  inner_fn <- glue("  .add_axis_to_encoding(spec, NA, '#/definitions/PositionFieldDef/properties/axis', encoding = '{enc}') ")


  ## Make the outer function
  make_function_helper(glue("remove_axis_{enc}"), docs, inner_fn, "spec")
}

create_remove_axis_functions <- function(schema) {
  ax_options <- get_enc_with_prop(schema, "axis")

  purrr::map_chr(ax_options,
    create_remove_axis_function,
    schema = schema
  )
}


create_stack_encoding_functions <- function(schema) {
  stack_doc <- make_option_group_doc(
    "stack_encoding",
    "stack",
    unlist(enums("#/definitions/StackOffset", schema)),
    "Add stack transform to encoding",
    "Add stack parameters to an encoding",
    na_option = TRUE
  )

  c(
    stack_doc,
    purrr::map_chr(get_enc_with_prop(schema, "stack"),
      create_stack_for_encoding,
      schema = schema
    )
  )
}



create_stack_for_encoding <- function(enc, schema) {
  make_option_function(
    "#/definitions/StackOffset",
    "stack",
    unlist(enums("#/definitions/StackOffset", schema)),
    glue("stack_{enc}"),
    ".add_stack_to_encoding",
    na_option = TRUE,
    pass_to_adder = list(encoding = enc),
    doc_group = "stack_encoding"
  )
}

create_aggregate_encoding_functions <- function(schema) {
  aggregate_doc <- make_option_group_doc(
    "aggregate_encoding",
    "aggregate",
    unlist(enums("#/definitions/AggregateOp", schema)),
    "Add aggregate transform to encoding",
    "Add aggregate parameters to an encoding",
    na_option = TRUE
  )

  c(
    aggregate_doc,
    purrr::map_chr(get_enc_with_prop(schema, "aggregate"),
      create_aggregate_for_encoding,
      schema = schema
    )
  )
}

create_aggregate_for_encoding <- function(enc, schema) {
  make_option_function(
    "#/definitions/Aggregate",
    "aggregate",
    unlist(enums("#/definitions/AggregateOp", schema)),
    glue("aggregate_{enc}"),
    ".add_aggregate_to_encoding",
    na_option = TRUE,
    pass_to_adder = list(encoding = enc),
    doc_group = "aggregate_encoding"
  )
}


create_sort_by_encoding_functions <- function(schema) {
  reference <- "#/definitions/SortByEncoding"
  doc_group <- "sort_encoding_by_encoding"

  doc <- make_group_doc(reference, schema,
    doc_group = doc_group,
    title = glue("Add sort transform by encoding to encoding"),
    description = "Add sort by encoding parameters to an encoding"
  )

  maker_func <- function(enc) {
    make_function(reference, schema, glue("sort_{enc}_by_encoding"), ".add_sort_to_encoding",
      pass_to_adder = list(encoding = enc), doc_group = doc_group
    )
  }

  c(
    doc,
    purrr::map_chr(get_enc_with_prop(schema, "sort"), maker_func)
  )
}

create_sort_by_field_functions <- function(schema) {
  reference <- "#/definitions/EncodingSortField"
  doc_group <- "sort_encoding_by_field"

  doc <- make_group_doc(reference, schema,
    doc_group = doc_group,
    title = glue("Add sort transform by field to encoding"),
    description = "Add sort by field parameters to an encoding"
  )

  maker_func <- function(enc) {
    make_function(reference, schema, glue("sort_{enc}_by_field"), ".add_sort_to_encoding",
      pass_to_adder = list(encoding = enc), doc_group = doc_group
    )
  }

  c(
    doc,
    purrr::map_chr(get_enc_with_prop(schema, "sort"), maker_func)
  )
}

create_sort_encoding_functions <- function(schema) {
  # A bit unique of a function so gets its own...

  doc_group <- "sort_encoding"

  title <- roxy_wrap("Add sorting to an encoding")
  desc <- roxy_wrap("Sort an encoding in 'ascending' or 'descending' order, or by given array")
  spec_doc <- glue("#' @param spec An input vega-lite spec")
  param_docs <- glue("#' @param value One of 'ascending', 'descending', a list with a custom ordering, or NA to specify no sorting")

  group_docs <- paste(
    make_docs_helper(title, desc, paste(spec_doc, param_docs, sep = "\n"), doc_group = doc_group, export = FALSE,),
    "NULL",
    "#> NULL",
    sep = "\n"
  )
  
  maker_func <- function(enc) {
    docs <- make_docs_for_group(doc_group)

    ## Make the inner function
    inner_fn <- glue("  .add_sort_to_encoding(spec, value, '#/definitions/Sort', encoding = '{enc}')")

    ## Get args
    args <- "spec, value"

    make_function_helper(glue("sort_{enc}"), docs, inner_fn, args)
  }

  c(
    group_docs,
    purrr::map_chr(get_enc_with_prop(schema, "sort"), maker_func)
  )
}


get_condition_references <- function(enc, schema) {
  objs <- props_grouped_by_object(glue("#/definitions/Encoding/properties/{enc}"), schema)
  refs <- paste0("#/definitions/", names(objs)[purrr::map_lgl(objs, ~ hasName(., "condition"))], "/properties/condition")
  refs
}

create_condition_encoding_functions <- function(schema) {

  # Creating the condition functions is a bit complicated because there are
  # multiple acceptable objects that can be inputs, and what those objects are
  # varies based on the Encoding.

  encs <- get_enc_with_prop(schema, "condition")

  refs <- purrr::map(encs, get_condition_references, schema = schema)
  names(refs) <- encs

  reference <- "#/definitions/EncodingSortField"
  doc_group <- "condition_encoding"

  doc <- make_group_doc(unique(unlist(refs, use.names = FALSE, recursive = TRUE)), schema,
    doc_group = doc_group,
    title = glue("Add conditioning to an encoding"),
    description = "Add condition parameters to an encoding"
  )

  c(
    doc,
    purrr::map_chr(encs, function(enc) {
      make_function(refs[[enc]],
        schema,
        glue("condition_{enc}"),
        glue(".add_condition_to_encoding"),
        doc_group = doc_group,
        pass_to_adder = list(encoding = enc)
      )
    })
  )
}
