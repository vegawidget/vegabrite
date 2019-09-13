.add_resolve <- function(spec, obj, ref, type, encoding){
  
  validate_sub_schema(obj, ref)
  if (is.null(spec[["resolve"]])) spec[["resolve"]] <- list()
  if (is.null(spec[["resolve"]][[type]])) spec[["resolve"]][[type]] <- list()
  spec[["resolve"]][[type]][[encoding]] <- obj
  
  return(spec)
}