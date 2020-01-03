#' Create API based on Vega-Lite schema
#'
#' Will auto-generate function definitions & documentation based on provided schema.
#' 
#' @param schema A JSON schema read into R as a list, e.g using [jsonlite::read_json()]
#'
#' @return A character vector with function definitions
#' @export
#'
#' @examples
#' 
#' schema_file <- Sys.glob(
#'     file.path(system.file("schema/vega-lite", package = "vegawidget"),
#'              "*.json")
#'  )
#' VL_SCHEMA <- jsonlite::read_json(schema_file)
#' api <- create_api(VL_SCHEMA)
create_api <- function(schema) {
  
  c(
    create_chart(schema),
    create_properties(schema),
    create_data_generic(schema),
    create_mark_functions(schema),
    create_encoding_functions(schema),
    create_transform_functions(schema),
    create_encoding_param_functions(schema),
    create_selection_functions(schema),
    create_binding_functions(schema),
    create_facet_functions(schema),
    create_repeat_functions(schema),
    create_resolve_functions(schema),
    create_config_functions(schema),
    create_additional_objects(schema)
  )
  
}

