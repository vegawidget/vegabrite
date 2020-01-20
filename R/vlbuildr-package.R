#' @keywords internal
#' @section Types of functions:
#' This package has three types of functions:
#' * `vl_chart` initializes a new vega-lite specification which can be build upon.
#' * Other `vl_` functions which add an object to an initial spec either from a pre-made 
#' input object (to the `.object`) parameter or created via the other parameters to the function.
#' See section below for more details on `.object` argument.
#' * Functions grouped in `vl` list and accessabile via `vl$`. 
#' These functions make an object (which can be added to a spec by a `vl_` function). Generally one 
#' does not need to use these functions as object can be created directly via the `vl_` functions,
#' but they can be useful for reusing components across charts or when additional flexibility is needed.
#' 
#' @section .object argument:
#' Most functions to add a component to a vegalite spec in this package have
#' as the first two arguments (1) the input spec to add onto, and (2) a `.object` 
#' parameter. The `.object` argument allows directly passing an object (generally a list) that
#' will be incorporated into the spec.
#' 
#' Additional arguments are avialable to enable creating an object directly from the 
#' function rather than having to pass it directly via `.object`. Generally, these arguments 
#' should not be used if the `.object` is provided. Thus if `.object` is passed as well as 
#' additional arguments, it is assumed that the `.object` input should be taken as the first 
#' optional parameter, and an error is given if the first optional parameter is already given.
#' 
#' For example, if a function `vl_add_something` has signature 
#' `vl_add_something(spec, .object, x = NULL, y = NULL, z = NULL)`
#' then if it is called with `vl_add_something('hello', z = 'world')` then the `'hello'` input 
#' is taken as the input to `x`. If `x` was already specified, e.g. `vl_add_something('hello', x = 'world')`
#' then an error would be raised.
#' @importFrom utils hasName
#' @importFrom utils modifyList
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
