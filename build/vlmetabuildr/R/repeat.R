
create_repeat_functions <- function(schema){
  purrr::map_chr(c("row","col","wrap"), create_repeat, schema = schema)
}

create_repeat <- function(type, schema) {
  
  reference <- "#/definitions/RepeatSpec/properties/repeat"
  suffix <- glue("repeat_{type}")
  
  spec_doc <- glue("#' @param spec An input vega-lite spec")
  extra_doc <- if (type == "wrap") "#' @param columns number of columns to add" else NULL
  param_docs <- "#' @param ... fields to use for repeat (strings)"
  
  docs <- make_docs_helper(
    glue("vl_{suffix}"),
    glue::glue("Add repeat to a vega-lite spec."),
    paste(spec_doc,extra_doc, param_docs, sep = "\n")
  )
  
  ## Make the inner function

  inner_fn <- if (type == "wrap") {
    glue("  .add_repeat(spec, list(...), '{reference}', columns = columns, .type = '{type}')")
  } else {
    glue("  .add_repeat(spec, list(...), '{reference}', .type = '{type}')")
  }
  
  ## Get args
  arg_list <-if (type == "wrap")  "spec, ..., columns = 2" else "spec, ..."
  
  ## Make the outer function
  make_function_helper(suffix, docs, inner_fn, arg_list)
  
}
