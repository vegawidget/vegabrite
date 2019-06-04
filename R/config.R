.add_sub_config <- function(spec, .config, ...){
  
  if (!hasName(spec,"config")) spec$config <- list()
  spec[["config"]][[.config]] <- list(...)

  return(spec)
}

.add_config <- function(spec, ...){
  
  
  if (!hasName(spec,"config")) spec$config <- list()
  spec[["config"]] <- utils::modifyList(spec[["config"]], list(...))
  
  return(spec)
}