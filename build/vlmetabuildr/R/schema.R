
# Use a schema path like "#/definitions/Encoding" to extract a portion of the
# schema
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

# Helper function to search through schema and apply a function to extract the
# elements needed. Function inputs check, get, and gather specify what is being
# looked for, what to extract, and how to combine results, respectively.
search <- function(schema, type, check, get, gather, base) {
  if (is.null(type)) {
    base()
  } else if (hasName(type, "$ref")) {
    search(schema, lookup(schema, type[["$ref"]]), check, get, gather, base)
  } else if (check(type)) {
    get(type)
  } else if ("anyOf" %in% names(type) || "allOf" %in% names(type) ||
    "oneOf" %in% names(type)) {
    t <- c(type[["anyOf"]], type[["allOf"]], type[["oneOf"]])
    gather(lapply(t, function(x) search(schema, x, check, get, gather, base)))
  } else {
    base()
  }
}

# A modified verision of 'search' helper that keeps track of the 'parent object' that lead to
# the result.
search2 <- function(schema, type, check, get, gather, base, parent) {
  if (is.null(type)) {
    base(type)
  } else if (hasName(type, "$ref")) {
    search2(schema, lookup(schema, type[["$ref"]]), check, get, gather, base, get_name_from_ref(type))
  } else if (check(type)) {
    out <- list()
    out[[parent]] <- get(type)
    out
  } else if ("anyOf" %in% names(type) || "allOf" %in% names(type) ||
    "oneOf" %in% names(type)) {
    t <- c(type[["anyOf"]], type[["allOf"]], type[["oneOf"]])
    ts <- purrr::map(t, function(x) search2(schema, x, check, get, gather, base, get_name_from_ref(x, parent)))
    gather(ts)
  } else {
    base(type)
  }
}

props <- function(ref, schema) {
  search(
    schema,
    list("$ref" = ref),
    function(x) {
      "type" %in% names(x) && x[["type"]] == "object"
    },
    function(x) {
      x[["properties"]]
    },
    function(x) unlist(x, recursive = FALSE),
    function() {
      NULL
    }
  )
}

types <- function(ref, schema) {
  search2(
    schema,
    list("$ref" = ref),
    function(x) {
      "type" %in% names(x) && x[["type"]] == "object"
    },
    function(x) {
      x
    },
    function(x) unlist(x, recursive = FALSE),
    function(x) {
      out <- list()
      out[[x[["type"]]]] <- x
      out
    },
    get_name_from_ref(ref)
  )
}

props_grouped_by_object <- function(ref, schema) {
  search2(
    schema,
    list("$ref" = ref),
    function(x) {
      "type" %in% names(x) && x[["type"]] == "object"
    },
    function(x) {
      x[["properties"]]
    },
    function(x) unlist(x, recursive = FALSE),
    function(x) {
      NULL
    },
    get_name_from_ref(ref)
  )
}

reqs <- function(ref, schema) {
  search(
    schema,
    list("$ref" = ref),
    function(x) {
      "type" %in% names(x) && x[["type"]] == "object"
    },
    function(x) {
      x[["required"]]
    },
    function(x) unlist(x),
    function() {
      NULL
    }
  )
}

enums <- function(ref, schema) {
  search(
    schema,
    list("$ref" = ref),
    function(x) {
      any(hasName(x, c("enum","anyOf")))
    },
    function(x) {
      if (hasName(x, "enum")) { 
        x[["enum"]]
      } else {
        vapply(x[["anyOf"]], get_name_from_ref, "")
      }
    },
    function(x) unlist(x, use.names = FALSE),
    function() list()
  )
}

additional_properties_allowed <- function(ref, schema) {
  if (length(ref) > 1) {
    return(any(purrr::map_lgl(ref, additional_properties_allowed, schema = schema)))
  }
  any(search(
    schema,
    list("$ref" = ref),
    function(x) {
      "type" %in% names(x) && x[["type"]] == "object"
    },
    function(x) {
      !identical(x[["additionalProperties"]], FALSE)
    },
    function(x) unlist(x, use.names = FALSE),
    function() list()
  ))
}

get_name_from_ref <- function(type, fallback = NULL) {
  if (hasName(type, "$ref")) {
    stringr::str_remove(type[["$ref"]], "#/definitions/")
  } else if (is.character(type)) {
    stringr::str_remove(type, "#/definitions/")
  } else {
    fallback
  }
}
