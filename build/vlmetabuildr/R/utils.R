deparse_c <- function(x) {
  paste(deparse(x, width.cutoff = 75L), collapse = "\n  ")
}

roxy_wrap <- function(x) {
  if (x == "") {
    return("#'")
  }
  paste(strwrap(x, width = 80, prefix = "#' "), collapse = "\n")
}

capitalize <- function(x) {
  first <- substring(x, first = 1L, last = 1L)
  rest <- substring(x, first = 2L, last = nchar(x))
  paste0(toupper(first), rest)
}

type_or_ref <- function(x) {
  if (hasName(x, "$ref")) {
    sp <- strsplit(x[["$ref"]], "/")[[1]]
    sp[length(sp)]
  } else if (hasName(x, "type")) {
    paste(x[["type"]], collapse = " OR ")
  } else {
    "Varies"
  }
}

get_description_terminal <- function(x) {
  if (hasName(x, "description")) {
    purrr::pluck(x, "description")
  } else if (hasName(x, "enum")) {
    paste(purrr::pluck(x, "enum"), collapse = ", ")
  } else if (hasName(x, "type")) {
    purrr::pluck(x, "type")
  } else if (hasName(x, "$ref")) {
    get_name_from_ref(x)
  } else {
    "?"
  }
}

get_description <- function(x) {
  if (hasName(x, "description")) {
    purrr::pluck(x, "description")
  } else if (hasName(x, "anyOf")) { 
    paste(unique(unlist(purrr::map(x[["anyOf"]], ~get_description_terminal(.)))), collapse = "\n\nOr: ")
  } else{
    get_description_terminal(x)
  }
}

get_description_plus <- function(x) {
  d <- get_description(x)
  d <- stringi::stri_wrap(stringr::str_split(d, "\n")[[1]], width = 80, whitespace_only = TRUE)
  d <- paste(d, collapse = "\n")
  d <- stringr::str_replace_all(d, "\n", "\n#' ")
  d
}

get_required_params <- function(schema, ref) {
  required <- unlist(purrr::map(ref, reqs, schema = schema))
  # Sort by frequency
  names(sort(table(required), decreasing = TRUE))
}

get_params <- function(schema, ref, exclude = NULL) {
  properties <- unlist(purrr::map(ref, props_grouped_by_object, schema = schema),
    recursive = FALSE
  )

  param_names <- unique(names(unlist(unname(properties), recursive = FALSE)))

  if (!is.null(exclude)) {
    param_names <- setdiff(param_names, exclude)
  }

  param_names
}

get_single_object_desc <- function(name, info) {
  if (name == "array") {
    glue("array of {get_name_from_ref(info[[\"items\"]])}")
  } else {
    name
  }
}

### This has been removed.
get_object_desc <- function(schema, ref) {
  object_types <- types(ref, schema)

  object_descs <- unique(purrr::map_chr(names(object_types), ~ get_single_object_desc(., object_types[[.]])))

  glue(
    "Directly input an object, rather than creating one via the other arguments. ",
    "Should not be used in conjunction with the other arguments other than `spec`. ",
    "Objects can be of type: ",
    glue_collapse(object_descs, sep = ", ", last = " or ")
  )
}

get_param_docs <- function(schema, ref, exclude = NULL) {
  properties <- unlist(purrr::map(ref, props_grouped_by_object, schema = schema),
    recursive = FALSE
  )

  param_names <- get_params(schema, ref, exclude)

  get_desc <- function(param) {
    objs <- unique(names(properties[purrr::map_lgl(properties, ~ hasName(., param))]))
    d <- purrr::map_chr(objs, ~ get_description_plus(properties[[.]][[param]]))
    grps <- split(objs, d)
    grp_descs <- names(grps)
    
    if (length(grp_descs) > 1) {
      return(paste(grp_descs, collapse = "\n#' \n#' Or: "))
    }
    grp_descs
  }

  param_desc <- purrr::map_chr(param_names, get_desc)


  # Some links have the '-' replaced with '%E2%80%93' -- replace
  param_desc <- stringr::str_replace_all(
    param_desc,
    "%E2%80%93",
    "-"
  )

  # Escape [ ] used when not in a link
  param_desc <- stringr::str_replace_all(
    param_desc,
    "(?<!`)\\[([^[\\]_]]*)\\](?![\\(`])",
    "\\\\\\[\\1\\\\\\]"
  )
  
  # Particular case -- To do: whether a coherent way 
  # to generalize with above
  param_desc <- stringr::str_replace_all(
    param_desc,
    "`\\[mousedown, window:mouseup\\]\n#' > window:mousemove!`",
    "\n#' ```\n#' [mousedown, window:mouseup] > window:mousemove!\n#' ```\n#' "
  )
  
  # Very specific fix for an issue...
  param_desc <- stringr::str_replace_all(
    param_desc,
    "`'Count of Records`",
    "`Count of Records`"
  )
  

  if (additional_properties_allowed(ref, schema)) {
    param_names <- c(param_names, "...")
    param_desc <- c(param_desc, "Additional elements for constructing object.")
  }

  paste("#' @param", param_names, param_desc, sep = " ", collapse = "\n")
}

get_object_doc <- function(schema, ref) {
  object_desc <- get_object_desc(schema, ref)
  glue("#' @param .object {object_desc}")
}
