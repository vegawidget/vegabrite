
#encoding_options <- props(VL_SCHEMA, list("$ref" = "#/definitions/Encoding"))

add_encoding <- function(spec, .enc, ...){

  if (is_composite(spec)) {
    stop("Can't add encoding to composite spec; add encoding prior to combining specs")
  }
  if (hasName(spec, "spec")) {
    inner_spec <- TRUE
    outer_spec <- spec
    spec <- outer_spec[["spec"]]
  } else {
    inner_spec <- FALSE
  }

  if (!hasName(spec,"encoding")) spec$encoding <- list()
  spec[["encoding"]][[.enc]] <- list(...)

  if (inner_spec) {
    outer_spec[["spec"]] <- spec
    spec <- outer_spec
  }
  return(spec)
}

pass_call <- function(func, exclude = list(), add = list(), env = parent.frame()){
  ## Get all args passed
  mc <- match.call(definition = sys.function(1),
                   call = sys.call(1))
  exclude_ix <- match(exclude,names(mc))
  exclude_ix <- exclude_ix[!is.na(exclude_ix)]
  if (length(exclude_ix) > 0) {
    mc <- mc[-exclude_ix]
  }
  for (new_arg in names(add)) {
    if (new_arg == "") stop("Must pass named arguments only")
    mc[new_arg] <- add[new_arg]
  }
  mc[[1]] <- func
  eval(mc, env)
}

type_or_ref <- function(x){
  if (hasName(x,"$ref")){
    sp <- strsplit(x[["$ref"]],"/")[[1]]
    sp[length(sp)]
  } else if (hasName(x,"type")){
    paste(x[["type"]],collapse = " OR ")
  } else {
    "Varies"
  }
}

get_param_docs <- function(properties) {

  d <- purrr::map_chr(properties, ~purrr::pluck(.,"description"))
  d <- stringr::str_replace_all(d, "\n","\n#' ")
  t <- purrr::map_chr(properties, type_or_ref)

  docs <- paste(d, glue("(type = {t})"))

  param_names <- unique(names(properties))

  param_desc <- purrr::map_chr(
    param_names,
    ~ paste(unique(docs[which(names(properties) == .)]), collapse = " OR ")
  )

  paste("#' @param", param_names, param_desc, sep = " ", collapse = "\n")

}

create_encoder <- function(schema, enc) {

  # Get all props...
  encode_props <- props(schema, list("$ref" = glue("#/definitions/Encoding/properties/{enc}")))
  encode_args <- paste(names(encode_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(encode_args)), collapse = ", ")

  param_docs <- get_param_docs(encode_props)

  glue("#' vl_encode_{enc}\n#' \n",
       "#' Add encoding for {enc} to a vega-lite spec.\n",
       "#' @param spec A vega-lite spec\n",
       "{param_docs}\n",
       "#' @return A modified spec\n",
      "vl_encode_{enc} <- function({arg_list}) {{\n",
       "  pass_call(quote(add_encoding), add = list(.enc = '{enc}'))\n",
       "}}\n")

}
