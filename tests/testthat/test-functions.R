
test_that("can add properties to a chart", {
  
  
  chart <- vl_chart() %>%
    vl_add_properties(height = 300, width = 50)
  
  expect_equivalent(chart$height, 300)
  expect_equivalent(chart$width, 50)
})
