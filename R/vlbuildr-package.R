#' @keywords internal
#' @section Types of functions:
#' This package has three types of functions:
#' * `vl_chart` initializes a new Vega-Lite specification which can be build upon.
#' * Other `vl_` functions which build up an object for a component of the spec and add it
#' into the input spec. These functionals enable iteratively building up all the different parts of a
#' spec. 
#'  
#' * Functions grouped in `vl` list and accessabile via `vl$`.
#' These functions make an object repressing some sub-component of a spec. These can be used to build
#' up an object that gets passed to a `vl_` function argument in cases where that argument is an object 
#' itself. Such objects can also be made as simple lists, but these helpers can be used for 
#' clarity and for the benefit of seeing what fields are available.
#' 
#' The goal of the package is to add sufficient functions that most common use cases won't 
#' require using these `vl$` functions, but they can be helpful for more complex charts.
#'
#' @importFrom utils hasName
#' @importFrom utils modifyList
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
