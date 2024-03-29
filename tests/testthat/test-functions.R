
test_that("can add properties to a chart", {
  chart <- vl_chart() %>%
    vl_add_properties(height = 300, width = 50)

  expect_equivalent(chart$height, 300)
  expect_equivalent(chart$width, 50)
})


test_that("can use vl_facet with columns argument", {
  chart <- vl_chart() %>%
    vl_facet(field = "x", type = "quantitative", columns = 2)

  expect_equivalent(chart$columns, 2)
  expect_equivalent(chart$facet, list(field = "x", type = "quantitative"))
})

test_that("can use vl_encode with row", {
  chart <- vl_chart() %>%
    vl_encode(row = list(field = "x", type = "quantitative"))

  expect_equivalent(chart$encoding$row, list(field = "x", type = "quantitative"))
})

test_that("can use vl_encode with column", {
  chart <- vl_chart() %>%
    vl_encode(column = list(field = "x", type = "quantitative"))

  expect_equivalent(chart$encoding$column, list(field = "x", type = "quantitative"))
})

test_that("can use vl_encode with facet", {
  chart <- vl_chart() %>%
    vl_encode(facet = list(field = "x", type = "quantitative"))

  expect_equivalent(chart$encoding$facet, list(field = "x", type = "quantitative"))
})

test_that("encode functions escapes field names with .", {
  chart <- vl_chart() %>%
    vl_encode_x(field = "abba.daba.doo", type = "quantitative")

  expect_equivalent(chart$encoding$x$field, "abba\\.daba\\.doo")
})

test_that("encode functions work with repeat", {
  chart <- vl_chart() %>%
    vl_encode_x(field = list("repeat" = "column"), type = "nominal")

  expect_equivalent(chart$encoding$x$field, list("repeat" = "column"))
})

test_that("mark sugar turns data arg into list", {
  chart <- vl_chart() %>%
    vl_mark_point(tooltip = "data")
  
  expect_equivalent(chart$mark$tooltip, list("content" = "data"))
})

test_that("mark sugar turns data arg into list", {
  chart <- vl_chart() %>%
    vl_mark_point(tooltip = "encoding")
  
  expect_equivalent(chart$mark$tooltip, list("content" = "encoding"))
})

test_that("mark sugar leaves other strings intact other than data, encoding", {
  chart <- vl_chart() %>%
    vl_mark_point(tooltip = "hello")
  
  expect_equivalent(chart$mark$tooltip, "hello")
})


