lookup <- function(schema, ref = NULL){
  if (is.null(ref)) return(NULL)
  path <- strsplit(ref,"/")[[1]]
  for (i in path) {
    if (i != "#") schema <- schema[[i]]
  }
  return(schema)
}

search <- function(schema, type, check, get, gather, base) {
  if (is.null(type)) {
    base()
  } else if (hasName(type,"$ref")) {
    search(schema, lookup(schema, type[["$ref"]]), check, get, gather, base)
  } else if (check(type)) {
    get(type)
  } else if ("anyOf" %in% names(type) || "allOf" %in% names(type) ||
             "oneOf" %in% names(type)) {
    t <- c(type[["anyOf"]],type[["allOf"]],type[["oneOf"]])
    gather(lapply(t, function(x) search(schema, x, check, get, gather, base)))
  } else{
    base()
  }
}

search2 <- function(schema, type, check, get, gather, base, parent) {
  if (is.null(type)) {
    base()
  } else if (hasName(type,"$ref")) {
    search2(schema, lookup(schema, type[["$ref"]]), check, get, gather, base, get_name_from_ref(type))
  } else if (check(type)) {
    out <- list()
    out[[parent]] <- get(type)
    out
  } else if ("anyOf" %in% names(type) || "allOf" %in% names(type) ||
             "oneOf" %in% names(type)) {
    t <- c(type[["anyOf"]],type[["allOf"]],type[["oneOf"]])
    ts <- purrr::map(t, function(x) search2(schema, x, check, get, gather, base, get_name_from_ref(t)))
    gather(ts)
  } else{
    base()
  }
}



#' schema utility functions
#'
#' @param schema imported json schema
#' @param type schema reference
#'
#' @return list
#' @export
#' @title schema
#' @name schema
#'
#' @examples
#' 
#' schema_file <- Sys.glob(file.path(system.file("schema/vega-lite", package = "vegawidget"),"*.json"))
#' VL_SCHEMA <- jsonlite::read_json(schema_file)
#' encoding_options <- props(VL_SCHEMA, list("$ref" = "#/definitions/Encoding"))
props <- function(schema, type) {
  search(schema,
         type,
         function(x) {"type" %in% names(x) && x[["type"]] ==  'object'},
         function(x) {x[["properties"]]},
         function(x) unlist(x, recursive = FALSE),
         function() {NULL}
  )
}

#' @name schema
#' @export
props2 <- function(schema, type) {
  search2(schema,
         type,
         function(x) {"type" %in% names(x) && x[["type"]] ==  'object'},
         function(x) {x[["properties"]]},
         function(x) unlist(x, recursive = FALSE),
         function() {NULL},
         get_name_from_ref(type)
  )
}

#' @name schema
#' @export
reqs <- function(schema, type) {
  search(schema,
         type,
         function(x) {"type" %in% names(x) && x[["type"]] ==  'object'},
         function(x) {x[["required"]]},
         function(x) unlist(x),
         function() {NULL}
  )
}

#' @name schema
#' @export
reqs2 <- function(schema, type) {
  search2(schema,
          type,
          function(x) {"type" %in% names(x) && x[["type"]] ==  'object'},
          function(x) {x[["required"]]},
          function(x) unlist(x, recursive = FALSE),
          function() {NULL},
          get_name_from_ref(type)
  )
}

#' @name schema
#' @export
enums <- function(schema, type) {
  search(schema,
         type,
         function(x) {"enum" %in% names(x)},
         function(x) {x[["enum"]]},
         function(x) unlist(x, use.names = FALSE),
         function() list()
  )
}



#' @name schema
#' @export
types <- function(schema, type) {
  search(schema,
         type,
         function(x) {"type" %in% names(x) && x[["type"]] ==  'object' &&
             "properties" %in% names(x) && "type" %in% names(x[["properties"]])},
         function(x) {x[["properties"]][["type"]][["enum"]]},
         function(x) sort(unlist(x, use.names = FALSE)),
         function() {c()}
  )
}



#' @name schema
#' @export
get_name_from_ref <- function(type){
  if (hasName(type,"$ref")) {
    stringr::str_remove(type[["$ref"]],"#/definitions/")
  } else {
    NULL
  }
}