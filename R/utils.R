# capitalize <- function(x){
#   first <- substring(x, first = 1L, last = 1L)
#   rest <- substring(x, first = 2L, last = nchar(x))
#   paste0(toupper(first),rest)
# }

#create_function(fn_name, args, dots = FALSE)

is_composite <- function(spec){

  if (hasName(spec, "spec")) {
    any(c("vconcat","hconcat","concat","layer") %in% names(spec[["spec"]]))
  } else{
    any(c("vconcat","hconcat","concat","layer") %in% names(spec))
  }

}



