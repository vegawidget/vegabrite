
test_that("can use shorthand type specification for nominal", {
  chart <- vl_chart() %>%
    vl_encode_x(field = "abba:N")

  expect_equivalent(chart$encoding$x$field, "abba")
  expect_equivalent(chart$encoding$x$type, "nominal")
})

test_that("can use shorthand type specification for temporal", {
  chart <- vl_chart() %>%
    vl_encode_x(field = "abba:T")

  expect_equivalent(chart$encoding$x$field, "abba")
  expect_equivalent(chart$encoding$x$type, "temporal")
})

test_that("can use shorthand type specification for quantitative", {
  chart <- vl_chart() %>%
    vl_encode_x(field = "abba:Q")

  expect_equivalent(chart$encoding$x$field, "abba")
  expect_equivalent(chart$encoding$x$type, "quantitative")
})

test_that("can use shorthand type specification for ordinal", {
  chart <- vl_chart() %>%
    vl_encode_x(field = "abba:O")

  expect_equivalent(chart$encoding$x$field, "abba")
  expect_equivalent(chart$encoding$x$type, "ordinal")
})

test_that("can use shorthand type specification in type field", {
  chart <- vl_chart() %>%
    vl_encode_x(field = "abba", type = "N")
  
  expect_equivalent(chart$encoding$x$field, "abba")
  expect_equivalent(chart$encoding$x$type, "nominal")
})

test_that("can use shorthand type with .object input", {
  chart <- vl_chart() %>%
    vl_encode_x("abba:O")

  expect_equivalent(chart$encoding$x$field, "abba")
  expect_equivalent(chart$encoding$x$type, "ordinal")
})

test_that("can use shorthand type with a list of inputs", {
  chart <- vl_chart() %>%
    vl_encode_tooltip(c("abba:O","baba:N"))
  
  expect_equivalent(chart$encoding$tooltip[[1]]$field, "abba")
  expect_equivalent(chart$encoding$tooltip[[1]]$type, "ordinal")
  expect_equivalent(chart$encoding$tooltip[[2]]$field, "baba")
  expect_equivalent(chart$encoding$tooltip[[2]]$type, "nominal")
})

test_that("can use shorthand type with a list of lists of inputs", {
  chart <- vl_chart() %>%
    vl_encode_tooltip(list(list(field = "abba:O"),list(field = "baba:N")))
  
  expect_equivalent(chart$encoding$tooltip[[1]]$field, "abba")
  expect_equivalent(chart$encoding$tooltip[[1]]$type, "ordinal")
  expect_equivalent(chart$encoding$tooltip[[2]]$field, "baba")
  expect_equivalent(chart$encoding$tooltip[[2]]$type, "nominal")
})

test_that("can use shorthand for facets", {
  chart <- vl_chart() %>%
    vl_facet_row(field = "abba:O")
  
  expect_equivalent(chart$facet$row$field, "abba")
  expect_equivalent(chart$facet$row$type, "ordinal")
})


test_that("can use shorthand for facets as first arg", {
  chart <- vl_chart() %>%
    vl_facet_row("abba:O")
  
  expect_equivalent(chart$facet$row$field, "abba")
  expect_equivalent(chart$facet$row$type, "ordinal")
})


