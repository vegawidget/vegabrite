context("examples")

test_writer <- function(plot, file, title = "") {
  vw_write_svg(plot, file)
}

simple_scatterplot <- vl_chart() %>%
  vl_add_data(values = mtcars) %>%
  vl_mark_point() %>%
  vl_encode_x("wt") %>%
  vl_encode_y("mpg") 

vdiffr::expect_doppelganger(
  "Simple scatterplot", 
  simple_scatterplot, 
  writer = test_writer
)

repeat_scatterplot <- vl_chart() %>%
  vl_add_data(values = mtcars) %>%
  vl_mark_point() %>%
  vl_encode_x(field = "hp", type = "quantitative") %>%
  vl_encode_y(field = "mpg", type = "quantitative") %>%
  vl_encode_color(field = list("repeat" = "column"), type = "nominal") %>%
  vl_repeat_col("cyl", "gear") %>%
  vl_resolve_scale_color("independent")

vdiffr::expect_doppelganger(
  "Repeat scatterplot", 
  repeat_scatterplot, 
  writer = test_writer
)


