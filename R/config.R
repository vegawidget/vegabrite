.add_sub_config <- function(spec, obj, ref, .config) {
  if (!hasName(spec, "config")) spec$config <- list()
  spec[["config"]][[.config]] <- obj

  return(spec)
}

.add_config <- function(spec, obj, ref) {
  if (!hasName(spec, "config")) spec$config <- list()
  spec[["config"]] <- utils::modifyList(spec[["config"]], obj)

  return(spec)
}
