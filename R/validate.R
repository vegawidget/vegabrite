

validate_sub_schema_inner <- function(x, ref) {
  sub_schema <- lookup(env$VL_SCHEMA, ref)

  valid <- is_valid_subschema(x, sub_schema, env$VL_SCHEMA)
  valid
}

validate_sub_schema <- function(x, ref) {
  valid <- any(vapply(ref, function(y) validate_sub_schema_inner(x, y), TRUE))

  if (!valid) {
    fn <- sys.calls()[[sys.nframe() - 2]][[1]]
    warning("Invalid schema for object passed to or created by ", fn, call. = FALSE)
  }
  return(valid)
}

lookup <- function(schema, ref = NULL) {
  if (is.null(ref)) {
    return(NULL)
  }
  path <- strsplit(ref, "/")[[1]]
  for (i in path) {
    if (i != "#") schema <- schema[[i]]
  }
  return(schema)
}


is_valid_string <- function(x, enum = NULL) {
  if (length(x) > 1) {
    return(FALSE)
  }
  if (is.null(enum)) {
    is.character(x)
  } else {
    x %in% enum
  }
}

is_valid_array <- function(x, items, schema) {
  # Can make more efficient by early exit...
  if (hasName(items, "anyOf")) {
    any(vapply(items$anyOf, function(any_of) is_valid_array(x, any_of, schema)))
  } else {
    all_return_true(x, function(y) is_valid_subschema(y, items, schema))
  }
}

all_return_true <- function(x, fun) {
  for (el in x) {
    if (identical(fun(el), FALSE)) {
      return(FALSE)
    }
  }
  return(TRUE)
}


is_valid_object <- function(obj, sub_schema, schema) {
  if (!is.list(obj)) {
    return(FALSE)
  }
  if (isTRUE(all.equal(sub_schema, list(type = "object")))) {
    return(TRUE)
  }

  required <- sub_schema$required
  has_required <- is.null(required) || all_return_true(required, function(x) hasName(obj, x))
  if (!has_required) {
    return(FALSE)
  }

  if (identical(sub_schema$additionalProperties, FALSE)) {
    if (any(!names(obj) %in% names(sub_schema$properties))) {
      return(FALSE)
    }
  }

  # If additional Properties is a ref... should pull in all the allowed properties there
  # For now, ignoring additional property validation...

  # Validate each property
  in_props <- intersect(names(obj), names(sub_schema$properties))
  all_return_true(in_props, function(x) is_valid_subschema(obj[[x]], sub_schema$properties[[x]], env$VL_SCHEMA))
}

is_valid_subschema <- function(obj, sub_schema, schema) {
  if (hasName(sub_schema, "$ref")) {
    return(is_valid_subschema(obj, lookup(schema, sub_schema[["$ref"]]), schema))
  }

  if (hasName(sub_schema, "anyOf")) {
    possible <- sub_schema[["anyOf"]]
    valid <- vapply(possible, function(x) is_valid_subschema(obj, x, schema), FALSE)
    return(any(valid))
  }

  if (!hasName(sub_schema, "type")) {
    return("AAAH")
  }
  
  if (length(sub_schema[["type"]]) > 1) {
    valid <- vapply(sub_schema[["type"]], function(x) is_valid_subschema(obj, list(type = x), schema), FALSE)
    return(any(valid))
  }

  switch(sub_schema[["type"]],
    object = is_valid_object(obj, sub_schema, schema),
    array = is_valid_array(obj, sub_schema[["items"]], schema),
    boolean = is.logical(obj),
    string = is_valid_string(obj, sub_schema[["enum"]]),
    null = is.na(obj) && length(obj) == 1,
    number = is.numeric(obj)
  )
}

