.add_resolve <- function(spec, obj, ref, type, encoding){
  
  vvalidate_sub_schema(obj, ref)
  spec[["resolve"]][[type]][[encoding]] <- obj$how
  
  return(spec)
}