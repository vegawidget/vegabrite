

validate_sub_schema <- function(x, ref) {
  
  sub_schema <- lookup(env$VL_SCHEMA, ref)
  
  is_valid_subschema(sub_schema, x, env$VL_SCHEMA)
}


lookup <- function(schema, ref = NULL){
  if (is.null(ref)) return(NULL)
  path <- strsplit(ref,"/")[[1]]
  for (i in path) {
    if (i != "#") schema <- schema[[i]]
  }
  return(schema)
}


is_valid_string <- function(x, enum = NULL) { 
  if (is.null(enum)) {
    is.character(x)
  } else {
    x %in% enum
  }
}

is_valid_array <- function(x, items, schema) {
  # Can make more efficient by early exit...
  if (hasName(items,"anyOf")) {
    any(vapply(items$anyOf, function(any_of) is_valid_array(x, any_of, schema) ))
  } else {
    all_return_true(x, function(y) is_valid_subschema(y, items, schema))
  }
}

all_return_true <- function(x, fun) {
  for (el in x) {
    if (!fun(el)) return(FALSE)
  }
 return(TRUE)
}


is_valid_object <- function(obj, sub_schema, schema) {
  
  if (!is.list(sub_schema)) return(FALSE)
  
  required <- sub_schema$required 
  has_required <- is.null(required) || all_return_true(required, function(x) hasName(obj, x))
  if (!has_required) return(FALSE)
  
  if (!sub_schema$additionalProperties) {
    if (any(!names(obj) %in% names(sub_schema$properties))) return(FALSE)
  }
  
  # Validate each property... 
  all_return_true(names(obj), function(x) is_valid_subschema(obj[[x]], sub_schema$properties[[x]], env$VL_SCHEMA))
}

is_valid_subschema <- function(obj, sub_schema, schema) {
  
  if (hasName(sub_schema,"$ref")) {
    return(is_valid_subschema(obj,  lookup(schema, sub_schema[["$ref"]]), schema))
  }
  
  if (hasName(sub_schema,"anyOf")) {
    possible = sub_schema[["anyOf"]]
    valid = vapply(possible, function(x) is_valid_subschema(obj, sub_schema, schema), FALSE)
    return(any(valid))
  }
  
  if (!hasName(sub_schema, "type")) {
    return("AAAH")
  }
  
  switch(sub_schema[["type"]],
         object = is_valid_object(obj, sub_schema,schema),
         array = is_valid_array(obj[["items"]], sub_schema, schema),
         boolean = is.logical(obj),
         string = is_valid_string(obj, sub_schema[["enum"]]),
         null = is.na(obj),
         number = is.numeric(obj)
         )
  
}

