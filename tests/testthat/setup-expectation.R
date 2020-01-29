list_sort <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x))) {
      x <- x[sort(names(x))]
    }
    return(lapply(x, list_sort))
  }
  return(x)
}

expect_equivalent_json <- function(object, reference) {

  # 0. Obliterate the 'schema' field...
  object["$schema"] <- NULL
  reference["$schema"] <- NULL

  # 1. Capture object and label
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")

  # 2. Call expect()
  eq <- all.equal(list_sort(object), list_sort(reference), check.attributes = FALSE)
  testthat::expect(
    isTRUE(eq),
    sprintf("%s does not match reference:\n%s", act$lab, eq)
  )

  # 3. Invisibly return the value
  invisible(act$val)
}
