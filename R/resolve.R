.add_resolve <- function(spec, .type, .enc, how){
  
  spec[["resolve"]][[.type]][[.enc]] <- how
  
  return(spec)
}