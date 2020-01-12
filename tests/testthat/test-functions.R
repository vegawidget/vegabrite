
test_that("can add properties to a chart", {
  
  
  chart <- vl_chart() %>%
    vl_add_properties(height = 300, width = 50)
  
  expect_equivalent(chart$height, 300)
  expect_equivalent(chart$width, 50)
})


test_that("can use vl_facet_wrap with columns argument", {
  
  chart <- vl_chart() %>%
    vl_facet_wrap(field = "x", type = "quantitative", columns = 2) 
  
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
    vl_encode_x(field = "abba.daba.doo", type = "quantitative" )
  
  expect_equivalent(chart$encoding$x$field, "abba\\.daba\\.doo")
  
  
})

test_that("encode functions work with repeat", {
  

  chart <- vl_chart() %>% 
    vl_encode_x(field = list("repeat" = "column"), type = "nominal" )
  
  expect_equivalent(chart$encoding$x$field, list("repeat" = "column"))
  
  
})

test_that("can use shorthand type specification for nominal", {
  chart <- vl_chart() %>% 
    vl_encode_x(field = "abba:N" )
  
  expect_equivalent(chart$encoding$x$field, "abba")
  expect_equivalent(chart$encoding$x$type, "nominal")

})

test_that("can use shorthand type specification for temporal", {
  chart <- vl_chart() %>% 
    vl_encode_x(field = "abba:T" )
  
  expect_equivalent(chart$encoding$x$field, "abba")
  expect_equivalent(chart$encoding$x$type, "temporal")
  
})

test_that("can use shorthand type specification for quantitative", {
  chart <- vl_chart() %>% 
    vl_encode_x(field = "abba:Q" )
  
  expect_equivalent(chart$encoding$x$field, "abba")
  expect_equivalent(chart$encoding$x$type, "quantitative")
  
})

test_that("can use shorthand type specification for ordinal", {
  chart <- vl_chart() %>% 
    vl_encode_x(field = "abba:O" )
  
  expect_equivalent(chart$encoding$x$field, "abba")
  expect_equivalent(chart$encoding$x$type, "ordinal")
  
})





