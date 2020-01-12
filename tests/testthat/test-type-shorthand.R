
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

test_that("can use shorthand type with .object input", {
  chart <- vl_chart() %>% 
    vl_encode_x("abba:O" )
  
  expect_equivalent(chart$encoding$x$field, "abba")
  expect_equivalent(chart$encoding$x$type, "ordinal")
  
})