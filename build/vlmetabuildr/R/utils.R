deparse_c <- function(x){
  paste(deparse(x, width.cutoff = 75L), collapse = "\n  ")
}

roxy_wrap <- function(x) {
  if (x == "") return("#'")
  paste(strwrap(x, width = 80, prefix = "#' "), collapse = "\n")
}

capitalize <- function(x){
  first <- substring(x, first = 1L, last = 1L)
  rest <- substring(x, first = 2L, last = nchar(x))
  paste0(toupper(first),rest)
}

type_or_ref <- function(x){
  if (hasName(x,"$ref")) {
    sp <- strsplit(x[["$ref"]],"/")[[1]]
    sp[length(sp)]
  } else if (hasName(x,"type")) {
    paste(x[["type"]],collapse = " OR ")
  } else {
    "Varies"
  }
}

get_description <- function(x) {
  if (hasName(x, "description")) {
    purrr::pluck(x,"description")
  } else if (hasName(x, "enum")) {
    paste(purrr::pluck(x,"enum"), collapse = ", ")
  } else {
    " "
  }
}

get_description_plus <- function(x) {

  d <- get_description(x)
  d <- stringr::str_replace_all(d, "\n","\n#' ")
  d

}

get_params <- function(schema, ref, exclude = NULL) {
  properties <- unlist(purrr::map(ref, props_grouped_by_object, schema = schema), 
                       recursive = FALSE)
  
  param_names <- unique(names(unlist(unname(properties), recursive = FALSE)))
  
  if (!is.null(exclude)) {
    param_names <- setdiff(param_names, exclude)
  }
  
  param_names
  
}

get_param_docs <- function(schema, ref, exclude = NULL) {
  
  properties <- unlist(purrr::map(ref, props_grouped_by_object, schema = schema), 
                       recursive = FALSE)
  
  param_names <- get_params(schema, ref, exclude)
  
  get_desc <- function(param) {
    objs <- names(properties[purrr::map_lgl(properties, ~hasName(., param))])
    d <- purrr::map_chr(objs, ~get_description_plus(properties[[.]][[param]]))
    grps <- split(objs, d)
    grp_descs <- purrr::map_chr(names(grps), 
                   ~glue('(_{paste(grps[[.]], collapse = ", ")}_) {.}'))
    if (length(grp_descs) > 1) {
      return(paste(grp_descs, collapse = "\n#' \n#' "))
    }
    grp_descs
  }
  
  param_desc <- purrr::map_chr(param_names, get_desc)
  
  #param_desc <- stringr::str_replace_all(
  #  param_desc,
  #  "_\\[([^[\\]_]]*)\\]_",
  #  "\\\\\\[\\1\\\\\\]")
  
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
    "\\\\\\[\\1\\\\\\]")
  
  # Very specific fix for an issue...
  param_desc <- stringr::str_replace_all(
    param_desc,
   "`'Count of Records`",
   "`Count of Records`")
  
  paste("#' @param", param_names, param_desc, sep = " ", collapse = "\n")
 
}

create_pass_function <- function(function_suffix, 
                                 recipient_function,
                                 arg_list,
                                 modify_args = "#'",
                                 doc_description = "#'",
                                 extra_docs = "#'",
                                 param_docs = "#'",
                                 group = NULL) {

  if (!is.null(group)) {
    template <- system.file(file.path("templates","template_pass_group.R"), 
                            package = 'vlmetabuildr')
  } else {
    template <- system.file(file.path("templates","template_pass.R"), 
                          package = 'vlmetabuildr')
  }
  
  glargs <- list(
    function_suffix = function_suffix,
    recipient_function = recipient_function,
    arg_list = arg_list,
    modify_args = modify_args,
    doc_description = doc_description,
    extra_docs = extra_docs,
    param_docs = param_docs
  )
  
  glargs['doc_name'] <- group
  
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}


create_custom_pass_function <- function(function_suffix, 
                                 recipient_function,
                                 arg_list,
                                 modify_args,
                                 doc_description = "#'",
                                 extra_docs = "#'",
                                 param_docs = "#'",
                                 group = NULL) {
  
  if (!is.null(group)) {
    template <- system.file(file.path("templates","template_pass_group_custom.R"), 
                            package = 'vlmetabuildr')
  } else {
    template <- system.file(file.path("templates","template_pass_custom.R"), 
                          package = 'vlmetabuildr')
  }
  glargs <- list(
    function_suffix = function_suffix,
    recipient_function = recipient_function,
    arg_list = arg_list,
    modify_args = modify_args,
    doc_description = doc_description,
    extra_docs = extra_docs,
    param_docs = param_docs
  )
  glargs['doc_name'] <- group
  
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}

# create_function_for_encode_param <- function(
#   enc,
#   param,
#   arg_list) {
# 
#   create_pass_function(
#     function_suffix = glue("{param}_{enc}"), 
#     recipient_function = glue(".add_{param}_to_encoding"),
#     arg_list = arg_list,
#     modify_args = glue("args_out <- c(args_out, list(.enc = '{enc}'))"),
#     extra_docs = glue("#' @seealso [vl_encode_{enc}()]"),
#     group = glue("{param}_encoding")
#   )
# 
# }

create_group_docs <- function(doc_name, 
                              doc_title,
                              doc_description = "#'",
                              extra_docs = "#'",
                              param_docs = "#'" ) {
  
  template <- system.file(file.path("templates","template_doc_group.R"), 
                          package = 'vlmetabuildr')
  glargs <- list(
    doc_name = doc_name,
    doc_title = doc_title,
    doc_description = doc_description,
    extra_docs = extra_docs,
    param_docs = param_docs
  )
  
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}
