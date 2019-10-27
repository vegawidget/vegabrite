
test_that("regression example 1 works", {
  
  vl_chart(width = 500, height = 300) %>%
    vl_add_data(url = "https://raw.githubusercontent.com/vega/vega/master/docs/data/income.json") %>%
    vl_lookup(lookup = "id", 
              from = list(data = list(url = "https://vega.github.io/vega-editor/app/data/us-10m.json",
                                      format = list(type = "topojson", feature = "states")
              ),
              key = "id"),
              as = "geo") %>%
    vl_add_projection_config(type = "albersUsa") %>%
    vl_mark_geoshape() %>%
    vl_encode_shape(field = "geo", type = "geojson") %>%
    vl_encode_color(field = "pct", type = "quantitative") %>%
    vl_encode(row=list(field = "group", type = "nominal")) %>%
    vw_as_json() %>% 
    expect_known_output("../regression/regression_example_1.json", print = TRUE)
  
})


test_that("regression example 2 works", {
  
  vl_chart(width =500, height = 300 ) %>%
    vl_add_data(url = "https://vega.github.io/vega-editor/app/data/us-10m.json",
                format= list(type="topojson",
                             feature="counties")) %>%
    vl_lookup(
      from=list(data=list(url="https://vega.github.io/vega-editor/app/data/unemployment.tsv"),
                key="id",
                fields=list("rate") ),
      lookup="id" ) %>%
    vl_add_projection_config(type = "albersUsa") %>%
    vl_mark_geoshape() %>%
    vl_encode_color(field = "rate",type = "quantitative") %>%
    vw_as_json() %>% 
    expect_known_output("../regression/regression_example_2.json", print = TRUE)
  
})




