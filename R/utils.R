
is_composite <- function(spec){

  if (hasName(spec, "spec")) {
    any(c("vconcat","hconcat","concat","layer") %in% names(spec[["spec"]]))
  } else{
    any(c("vconcat","hconcat","concat","layer") %in% names(spec))
  }

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

