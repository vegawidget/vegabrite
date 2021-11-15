#' @keywords internal
#' @section Types of functions:
#' This package has three types of functions:
#' * `vl_chart` initializes a new Vega-Lite specification which can be build upon.
#' * Other `vl_` functions which build up an object for a component of the spec and add it
#' into the input spec. These allow iteratively building up all the different parts of a
#' spec. Instead of using the function to build up the sub-component of the spec, one can alternatively
#' use the .object argument to pass in a pre-build object.  
#' * Functions grouped in `vl` list and accessabile via `vl$`.
#' These functions make an object (which can be added to a spec by a `vl_` function, or might help
#' create the value for an argument to such a function). Generally one
#' does not need to use these functions as object can be created directly via the `vl_` functions,
#' but they can be useful for reusing components across charts or when building up more complex
#' specs.
#'
#' @importFrom utils hasName
#' @importFrom utils modifyList
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
