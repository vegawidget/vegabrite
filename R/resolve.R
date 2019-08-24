.add_resolve <- function(spec, obj, .type, .enc){
  
  spec[["resolve"]][[.type]][[.enc]] <- obj$how
  
  return(spec)
}