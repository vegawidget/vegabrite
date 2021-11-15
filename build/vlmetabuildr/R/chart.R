create_chart <- function(schema) {
  reference <- "#/definitions/TopLevelSpec"
  suffix <- "chart"

  param_docs <- get_param_docs(schema, "#/definitions/TopLevelSpec")

  docs <- paste(param_docs, "#' @name vl_chart", sep = "\n")

  param_names <- get_params(schema, reference)
  chart_names <- unique(c("data", param_names))
  chart_names <- stringr::str_replace(chart_names, "^repeat$", "`repeat`")
  defaults <- rep("NULL", length(chart_names))
  defaults[which(chart_names == "$schema")] <- "vegawidget::vega_schema()"
  chart_names[which(chart_names == "$schema")] <- "`$schema`"
  chart_args <- paste(chart_names, defaults, sep = " = ")
  arg_list <- paste(chart_args, collapse = ", ")

  inner_fn <- "  obj <- as.list(environment())\n  obj <- .make_object(obj, NULL, NULL)\n  .chart_initialize(obj)"

  ## Make the outer function
  make_function_helper(suffix, docs, inner_fn, arg_list)
}


create_properties <- function(schema) {
  alt <- c(
    "layer", "repeat", "concat", "hconcat", "vconcat", "facet", "mark",
    "transform", "selection", "resolve", "data", "config", "$schema", "spec",
    "encoding"
  )

  suffix <- "add_properties"
  reference <- "#/definitions/TopLevelSpec"
  description <- paste0(
    "Add properties to top level of a vega-lite spec. Allows adding properties like width,",
    "height, background which don't have a specific function for adding them (unlike `mark`",
    "or `encoding`)."
  )

  docs <- make_docs(reference, schema, suffix,
    exclude_args = alt,
    description = description
  )

  ## Make the inner function
  inner_fn <- make_function_innards(reference, schema,
    override_args = NULL,
    adder_function = ".add_properties", pass_to_adder = NULL
  )

  ## Get args
  args <- make_arg_list(reference, schema, alt, NULL)

  ## Make the outer function
  fn <- glue::glue("vl_{suffix} <- function({args}){{\n{inner_fn}\n}}")

  # Combine docs and function
  glue::glue_collapse(c(docs, fn), sep = "\n", last = "\n")
}
