SCHEMA <- vegawidget::vega_schema()

## Example based on: https://vega.github.io/vega-lite/examples/bar.html

json1 <- '{
  "$schema": "https://vega.github.io/schema/vega-lite/v3.json",
  "description": "A simple bar chart with embedded data.",
  "data": {
    "values": [
      {"a": "A", "b": 28}, {"a": "B", "b": 55}, {"a": "C", "b": 43},
      {"a": "D", "b": 91}, {"a": "E", "b": 81}, {"a": "F", "b": 53},
      {"a": "G", "b": 19}, {"a": "H", "b": 87}, {"a": "I", "b": 52}
      ]
  },
  "mark": {"type": "bar"},
  "encoding": {
    "x": {"field": "a", "type": "ordinal"},
    "y": {"field": "b", "type": "quantitative"}
  }
}'




test_that("can build simple bar chart", {

  spec <- jsonlite::fromJSON(json1)
  data <- spec$data$values
  
  chart <- vl_chart(description = "A simple bar chart with embedded data.") %>%
    vl_add_data(values = data) %>%
    vl_mark_bar() %>%
    vl_encode_x(field = "a", type = "ordinal") %>%
    vl_encode_y(field = "b", type = "quantitative")
  
  expect_equivalent_json(chart, spec)
})

# Example based on https://vega.github.io/vega-lite/examples/bar_aggregate.html

json2 <- '
{
  "$schema": "https://vega.github.io/schema/vega-lite/v3.json",
  "description": "A bar chart showing the US population distribution of age groups in 2000.",
  "height": {"step": 17},
  "data": { "url": "data/population.json"},
  "transform": [{"filter": "datum.year == 2000"}],
  "mark": "bar",
  "encoding": {
    "y": {
      "field": "age", "type": "ordinal"
    },
    "x": {
      "aggregate": "sum", "field": "people", "type": "quantitative",
      "axis": {"title": "population"}
    }
  }
}
'

test_that("can build aggregate bar chart", {
  
  spec <- jsonlite::fromJSON(json2)
  data <- spec$data$url
  
  chart <- vl_chart(description = "A bar chart showing the US population distribution of age groups in 2000.", height = list(step = 17)) %>%
    vl_add_data(url = data) %>%
    vl_filter("datum.year == 2000") %>%
    vl_mark_bar() %>%
    vl_encode_y(field = "age", type = "ordinal") %>%
    vl_encode_x(field = "people", type = "quantitative") %>%
    vl_aggregate_x("sum") %>%
    vl_axis_x(title = "population") 
  
  expect_equivalent_json(chart, spec)
})

# https://vega.github.io/vega-lite/examples/bar_aggregate_sort_by_encoding.html

json3 <- '
{
  "$schema": "https://vega.github.io/schema/vega-lite/v3.json",
  "description": "A bar chart that sorts the y-values by the x-values.",
  "data": {"url": "data/population.json"},
  "transform": [{"filter": "datum.year == 2000"}],
  "height": {"step": 17},
  "mark": "bar",
  "encoding": {
    "y": {
      "field": "age",
      "type": "ordinal",
      "sort": {"endoding": "x", "order": "descending"}
    },
    "x": {
      "aggregate": "sum",
      "field": "people",
      "type": "quantitative",
      "axis": {"title": "population"}
    }
  }
}
'

test_that("can build aggregate bar chart (sorted)", {
  
  spec <- jsonlite::fromJSON(json3)
  data <- spec$data$url
  
  chart <- vl_chart(description = "A bar chart that sorts the y-values by the x-values.", height = list(step = 17)) %>%
    vl_add_data(url = data) %>%
    vl_filter("datum.year == 2000") %>%
    vl_mark_bar() %>%
    vl_encode_y(field = "age", type = "ordinal") %>%
    vl_sort_y_by_encoding(encoding = "x", order = "descending") %>%
    vl_encode_x(field = "people", type = "quantitative") %>%
    vl_aggregate_x("sum") %>%
    vl_axis_x(title = "population") 
  
  expect_equivalent_json(chart, spec)
})



