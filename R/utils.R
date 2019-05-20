
is_composite <- function(spec){

  if (hasName(spec, "spec")) {
    any(c("vconcat","hconcat","concat","layer") %in% names(spec[["spec"]]))
  } else{
    any(c("vconcat","hconcat","concat","layer") %in% names(spec))
  }

}

