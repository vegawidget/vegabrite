
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
  
  expect_equivalent(chart$facet$row, list(field = "x", type = "quantitative"))
})

test_that("can use vl_encode with column", {
  
  chart <- vl_chart() %>%
    vl_encode(column = list(field = "x", type = "quantitative")) 
  
  expect_equivalent(chart$facet$column, list(field = "x", type = "quantitative"))
})

test_that("can use vl_encode with facet", {
  
  chart <- vl_chart() %>%
    vl_encode(facet = list(field = "x", type = "quantitative")) 
  
  expect_equivalent(chart$facet, list(field = "x", type = "quantitative"))
})




