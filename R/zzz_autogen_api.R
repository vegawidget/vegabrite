
#' vl_encode_color
#' 
#' Add encoding for color to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = string OR null)
#' @return A modified spec
#' @export
vl_encode_color <- function(spec, aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'color'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_detail
#' 
#' Add encoding for detail to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @return A modified spec
#' @export
vl_encode_detail <- function(spec, aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'detail'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_fill
#' 
#' Add encoding for fill to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = string OR null)
#' @return A modified spec
#' @export
vl_encode_fill <- function(spec, aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'fill'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_fillOpacity
#' 
#' Add encoding for fillOpacity to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
vl_encode_fillOpacity <- function(spec, aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'fillOpacity'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_href
#' 
#' Add encoding for href to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = string)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = string)
#' @return A modified spec
#' @export
vl_encode_href <- function(spec, aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'href'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_key
#' 
#' Add encoding for key to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @return A modified spec
#' @export
vl_encode_key <- function(spec, aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'key'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_latitude
#' 
#' Add encoding for latitude to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = string)
#' @return A modified spec
#' @export
vl_encode_latitude <- function(spec, aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'latitude'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_latitude2
#' 
#' Add encoding for latitude2 to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @return A modified spec
#' @export
vl_encode_latitude2 <- function(spec, aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'latitude2'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_longitude
#' 
#' Add encoding for longitude to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = string)
#' @return A modified spec
#' @export
vl_encode_longitude <- function(spec, aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'longitude'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_longitude2
#' 
#' Add encoding for longitude2 to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @return A modified spec
#' @export
vl_encode_longitude2 <- function(spec, aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'longitude2'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_opacity
#' 
#' Add encoding for opacity to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
vl_encode_opacity <- function(spec, aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'opacity'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_order
#' 
#' Add encoding for order to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param sort The sort order. One of `"ascending"` (default) or `"descending"`. (type = SortOrder)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
vl_encode_order <- function(spec, aggregate = NULL, bin = NULL, field = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'order'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_shape
#' 
#' Add encoding for shape to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = TypeForShape)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = string)
#' @return A modified spec
#' @export
vl_encode_shape <- function(spec, aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'shape'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_size
#' 
#' Add encoding for size to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
vl_encode_size <- function(spec, aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'size'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_stroke
#' 
#' Add encoding for stroke to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = string OR null)
#' @return A modified spec
#' @export
vl_encode_stroke <- function(spec, aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'stroke'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_strokeOpacity
#' 
#' Add encoding for strokeOpacity to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
vl_encode_strokeOpacity <- function(spec, aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'strokeOpacity'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_strokeWidth
#' 
#' Add encoding for strokeWidth to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
vl_encode_strokeWidth <- function(spec, aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'strokeWidth'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_text
#' 
#' Add encoding for text to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param format The [formatting pattern](https://vega.github.io/vega-lite/docs/format.html) for a text field. If not defined, this will be determined automatically. (type = string)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = string OR number OR boolean)
#' @return A modified spec
#' @export
vl_encode_text <- function(spec, aggregate = NULL, bin = NULL, condition = NULL, field = NULL, format = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'text'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_tooltip
#' 
#' Add encoding for tooltip to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param format The [formatting pattern](https://vega.github.io/vega-lite/docs/format.html) for a text field. If not defined, this will be determined automatically. (type = string)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = string OR number OR boolean)
#' @return A modified spec
#' @export
vl_encode_tooltip <- function(spec, aggregate = NULL, bin = NULL, condition = NULL, field = NULL, format = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'tooltip'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_x
#' 
#' Add encoding for x to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param axis An object defining properties of axis's gridlines, ticks and labels.
#' If `null`, the axis for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [axis properties](https://vega.github.io/vega-lite/docs/axis.html) are applied. (type = Varies)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param impute An object defining the properties of the Impute Operation to be applied.
#' The field value of the other positional channel is taken as `key` of the `Impute` Operation.
#' The field of the `color` channel if specified is used as `groupby` of the `Impute` Operation. (type = ImputeParams)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param stack Type of stacking offset if the field should be stacked.
#' `stack` is only applicable for `x` and `y` channels with continuous domains.
#' For example, `stack` of `y` can be used to customize stacking for a vertical bar chart.
#' 
#' `stack` can be one of the following values:
#' - `"zero"`: stacking with baseline offset at zero value of the scale (for creating typical stacked [bar](https://vega.github.io/vega-lite/docs/stack.html#bar) and [area](https://vega.github.io/vega-lite/docs/stack.html#area) chart).
#' - `"normalize"` - stacking with normalized domain (for creating [normalized stacked bar and area charts](https://vega.github.io/vega-lite/docs/stack.html#normalized). <br/>
#' -`"center"` - stacking with center baseline (for [streamgraph](https://vega.github.io/vega-lite/docs/stack.html#streamgraph)).
#' - `null` - No-stacking. This will produce layered [bar](https://vega.github.io/vega-lite/docs/stack.html#layered-bar-chart) and area chart.
#' 
#' __Default value:__ `zero` for plots with all of the following conditions are true:
#' (1) the mark is `bar` or `area`;
#' (2) the stacked measure channel (x or y) has a linear scale;
#' (3) At least one of non-position channels mapped to an unaggregated field that is different from x and y.  Otherwise, `null` by default. (type = Varies)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = Varies)
#' @return A modified spec
#' @export
vl_encode_x <- function(spec, aggregate = NULL, axis = NULL, bin = NULL, field = NULL, impute = NULL, scale = NULL, sort = NULL, stack = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'x'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_x2
#' 
#' Add encoding for x2 to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = Varies)
#' @return A modified spec
#' @export
vl_encode_x2 <- function(spec, aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'x2'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_xError
#' 
#' Add encoding for xError to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
vl_encode_xError <- function(spec, aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'xError'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_xError2
#' 
#' Add encoding for xError2 to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
vl_encode_xError2 <- function(spec, aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'xError2'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_y
#' 
#' Add encoding for y to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param axis An object defining properties of axis's gridlines, ticks and labels.
#' If `null`, the axis for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [axis properties](https://vega.github.io/vega-lite/docs/axis.html) are applied. (type = Varies)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param impute An object defining the properties of the Impute Operation to be applied.
#' The field value of the other positional channel is taken as `key` of the `Impute` Operation.
#' The field of the `color` channel if specified is used as `groupby` of the `Impute` Operation. (type = ImputeParams)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param stack Type of stacking offset if the field should be stacked.
#' `stack` is only applicable for `x` and `y` channels with continuous domains.
#' For example, `stack` of `y` can be used to customize stacking for a vertical bar chart.
#' 
#' `stack` can be one of the following values:
#' - `"zero"`: stacking with baseline offset at zero value of the scale (for creating typical stacked [bar](https://vega.github.io/vega-lite/docs/stack.html#bar) and [area](https://vega.github.io/vega-lite/docs/stack.html#area) chart).
#' - `"normalize"` - stacking with normalized domain (for creating [normalized stacked bar and area charts](https://vega.github.io/vega-lite/docs/stack.html#normalized). <br/>
#' -`"center"` - stacking with center baseline (for [streamgraph](https://vega.github.io/vega-lite/docs/stack.html#streamgraph)).
#' - `null` - No-stacking. This will produce layered [bar](https://vega.github.io/vega-lite/docs/stack.html#layered-bar-chart) and area chart.
#' 
#' __Default value:__ `zero` for plots with all of the following conditions are true:
#' (1) the mark is `bar` or `area`;
#' (2) the stacked measure channel (x or y) has a linear scale;
#' (3) At least one of non-position channels mapped to an unaggregated field that is different from x and y.  Otherwise, `null` by default. (type = Varies)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = Varies)
#' @return A modified spec
#' @export
vl_encode_y <- function(spec, aggregate = NULL, axis = NULL, bin = NULL, field = NULL, impute = NULL, scale = NULL, sort = NULL, stack = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'y'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_y2
#' 
#' Add encoding for y2 to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = Varies)
#' @return A modified spec
#' @export
vl_encode_y2 <- function(spec, aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'y2'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_yError
#' 
#' Add encoding for yError to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
vl_encode_yError <- function(spec, aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'yError'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_encode_yError2
#' 
#' Add encoding for yError2 to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
vl_encode_yError2 <- function(spec, aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'yError2'))
  rlang::exec(.add_encoding, !!!args_out)
}
 
#' vl_Color
#' 
#' Create spec for color encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = string OR null)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_color()]
vl_Color <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_Detail
#' 
#' Create spec for detail encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_detail()]
vl_Detail <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_Fill
#' 
#' Create spec for fill encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = string OR null)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_fill()]
vl_Fill <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_FillOpacity
#' 
#' Create spec for fillOpacity encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_fillOpacity()]
vl_FillOpacity <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_Href
#' 
#' Create spec for href encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = string)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = string)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_href()]
vl_Href <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_Key
#' 
#' Create spec for key encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_key()]
vl_Key <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_Latitude
#' 
#' Create spec for latitude encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = string)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_latitude()]
vl_Latitude <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_Latitude2
#' 
#' Create spec for latitude2 encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_latitude2()]
vl_Latitude2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_Longitude
#' 
#' Create spec for longitude encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = string)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_longitude()]
vl_Longitude <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_Longitude2
#' 
#' Create spec for longitude2 encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_longitude2()]
vl_Longitude2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_Opacity
#' 
#' Create spec for opacity encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_opacity()]
vl_Opacity <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_Order
#' 
#' Create spec for order encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param sort The sort order. One of `"ascending"` (default) or `"descending"`. (type = SortOrder)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_order()]
vl_Order <- function(aggregate = NULL, bin = NULL, field = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_Shape
#' 
#' Create spec for shape encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = TypeForShape)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = string)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_shape()]
vl_Shape <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_Size
#' 
#' Create spec for size encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_size()]
vl_Size <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_Stroke
#' 
#' Create spec for stroke encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = string OR null)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_stroke()]
vl_Stroke <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_StrokeOpacity
#' 
#' Create spec for strokeOpacity encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_strokeOpacity()]
vl_StrokeOpacity <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_StrokeWidth
#' 
#' Create spec for strokeWidth encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param legend An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied. (type = Varies)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_strokeWidth()]
vl_StrokeWidth <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_Text
#' 
#' Create spec for text encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param format The [formatting pattern](https://vega.github.io/vega-lite/docs/format.html) for a text field. If not defined, this will be determined automatically. (type = string)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = string OR number OR boolean)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_text()]
vl_Text <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, format = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_Tooltip
#' 
#' Create spec for tooltip encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param condition One or more value definition(s) with a selection predicate.
#' 
#' __Note:__ A field definition's `condition` property can only contain [value definitions](https://vega.github.io/vega-lite/docs/encoding.html#value-def)
#' since Vega-Lite only allows at most one encoded field per encoding channel. (type = Varies) OR A field definition or one or more value definition(s) with a selection predicate. (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param format The [formatting pattern](https://vega.github.io/vega-lite/docs/format.html) for a text field. If not defined, this will be determined automatically. (type = string)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = string OR number OR boolean)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_tooltip()]
vl_Tooltip <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, format = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_X
#' 
#' Create spec for x encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param axis An object defining properties of axis's gridlines, ticks and labels.
#' If `null`, the axis for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [axis properties](https://vega.github.io/vega-lite/docs/axis.html) are applied. (type = Varies)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param impute An object defining the properties of the Impute Operation to be applied.
#' The field value of the other positional channel is taken as `key` of the `Impute` Operation.
#' The field of the `color` channel if specified is used as `groupby` of the `Impute` Operation. (type = ImputeParams)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param stack Type of stacking offset if the field should be stacked.
#' `stack` is only applicable for `x` and `y` channels with continuous domains.
#' For example, `stack` of `y` can be used to customize stacking for a vertical bar chart.
#' 
#' `stack` can be one of the following values:
#' - `"zero"`: stacking with baseline offset at zero value of the scale (for creating typical stacked [bar](https://vega.github.io/vega-lite/docs/stack.html#bar) and [area](https://vega.github.io/vega-lite/docs/stack.html#area) chart).
#' - `"normalize"` - stacking with normalized domain (for creating [normalized stacked bar and area charts](https://vega.github.io/vega-lite/docs/stack.html#normalized). <br/>
#' -`"center"` - stacking with center baseline (for [streamgraph](https://vega.github.io/vega-lite/docs/stack.html#streamgraph)).
#' - `null` - No-stacking. This will produce layered [bar](https://vega.github.io/vega-lite/docs/stack.html#layered-bar-chart) and area chart.
#' 
#' __Default value:__ `zero` for plots with all of the following conditions are true:
#' (1) the mark is `bar` or `area`;
#' (2) the stacked measure channel (x or y) has a linear scale;
#' (3) At least one of non-position channels mapped to an unaggregated field that is different from x and y.  Otherwise, `null` by default. (type = Varies)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = Varies)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_x()]
vl_X <- function(aggregate = NULL, axis = NULL, bin = NULL, field = NULL, impute = NULL, scale = NULL, sort = NULL, stack = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_X2
#' 
#' Create spec for x2 encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = Varies)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_x2()]
vl_X2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_XError
#' 
#' Create spec for xError encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_xError()]
vl_XError <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_XError2
#' 
#' Create spec for xError2 encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_xError2()]
vl_XError2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_Y
#' 
#' Create spec for y encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param axis An object defining properties of axis's gridlines, ticks and labels.
#' If `null`, the axis for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [axis properties](https://vega.github.io/vega-lite/docs/axis.html) are applied. (type = Varies)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param impute An object defining the properties of the Impute Operation to be applied.
#' The field value of the other positional channel is taken as `key` of the `Impute` Operation.
#' The field of the `color` channel if specified is used as `groupby` of the `Impute` Operation. (type = ImputeParams)
#' @param scale An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied. (type = Varies)
#' @param sort Sort order for the encoded field.
#' 
#' For continuous fields (quantitative or temporal), `sort` can be either `"ascending"` or `"descending"`.
#' 
#' For discrete fields, `sort` can be one of the following:
#' - `"ascending"` or `"descending"` -- for sorting by the values' natural order in Javascript.
#' - [A sort-by-encoding definition](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding) for sorting by another encoding channel. (This type of sort definition is not available for `row` and `column` channels.)
#' - [A sort field definition](https://vega.github.io/vega-lite/docs/sort.html#sort-field) for sorting by another field.
#' - [An array specifying the field values in preferred order](https://vega.github.io/vega-lite/docs/sort.html#sort-array). In this case, the sort order will obey the values in the array, followed by any unspecified values in their original order.  For discrete time field, values in the sort array can be [date-time definition objects](types#datetime). In addition, for time units `"month"` and `"day"`, the values can be the month or day names (case insensitive) or their 3-letter initials (e.g., `"Mon"`, `"Tue"`).
#' - `null` indicating no sort.
#' 
#' __Default value:__ `"ascending"`
#' 
#' __Note:__ `null` is not supported for `row` and `column`. (type = Sort)
#' @param stack Type of stacking offset if the field should be stacked.
#' `stack` is only applicable for `x` and `y` channels with continuous domains.
#' For example, `stack` of `y` can be used to customize stacking for a vertical bar chart.
#' 
#' `stack` can be one of the following values:
#' - `"zero"`: stacking with baseline offset at zero value of the scale (for creating typical stacked [bar](https://vega.github.io/vega-lite/docs/stack.html#bar) and [area](https://vega.github.io/vega-lite/docs/stack.html#area) chart).
#' - `"normalize"` - stacking with normalized domain (for creating [normalized stacked bar and area charts](https://vega.github.io/vega-lite/docs/stack.html#normalized). <br/>
#' -`"center"` - stacking with center baseline (for [streamgraph](https://vega.github.io/vega-lite/docs/stack.html#streamgraph)).
#' - `null` - No-stacking. This will produce layered [bar](https://vega.github.io/vega-lite/docs/stack.html#layered-bar-chart) and area chart.
#' 
#' __Default value:__ `zero` for plots with all of the following conditions are true:
#' (1) the mark is `bar` or `area`;
#' (2) the stacked measure channel (x or y) has a linear scale;
#' (3) At least one of non-position channels mapped to an unaggregated field that is different from x and y.  Otherwise, `null` by default. (type = Varies)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param type The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
#' It can also be a `"geojson"` type for encoding ['geoshape'](https://vega.github.io/vega-lite/docs/geoshape.html).
#' 
#' 
#' __Note:__
#' 
#' - Data values for a temporal field can be either a date-time string (e.g., `"2015-03-07 12:32:17"`, `"17:01"`, `"2015-03-16"`. `"2015"`) or a timestamp number (e.g., `1552199579097`).
#' - Data `type` describes the semantics of the data rather than the primitive data types (`number`, `string`, etc.). The same primitive data type can have different types of measurement. For example, numeric data can represent quantitative, ordinal, or nominal data.
#' - When using with [`bin`](https://vega.github.io/vega-lite/docs/bin.html), the `type` property can be either `"quantitative"` (for using a linear bin scale) or [`"ordinal"` (for using an ordinal bin scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`timeUnit`](https://vega.github.io/vega-lite/docs/timeunit.html), the `type` property can be either `"temporal"` (for using a temporal scale) or [`"ordinal"` (for using an ordinal scale)](https://vega.github.io/vega-lite/docs/type.html#cast-bin).
#' - When using with [`aggregate`](https://vega.github.io/vega-lite/docs/aggregate.html), the `type` property refers to the post-aggregation data type. For example, we can calculate count `distinct` of a categorical field `"cat"` using `{"aggregate": "distinct", "field": "cat", "type": "quantitative"}`. The `"type"` of the aggregate output is `"quantitative"`.
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`). (type = StandardType)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = Varies)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_y()]
vl_Y <- function(aggregate = NULL, axis = NULL, bin = NULL, field = NULL, impute = NULL, scale = NULL, sort = NULL, stack = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_Y2
#' 
#' Create spec for y2 encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = Varies)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_y2()]
vl_Y2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_YError
#' 
#' Create spec for yError encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_yError()]
vl_YError <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_YError2
#' 
#' Create spec for yError2 encoding.
#' @param aggregate Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None) (type = Aggregate)
#' @param bin A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - To indicate that the data for the `x` (or `y`) channel are already binned, you can set the `bin` property of the `x` (or `y`) channel to `"binned"` and map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false` (type = Varies)
#' @param field __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo['bar']"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\[0\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`. (type = Field)
#' @param timeUnit Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None) (type = TimeUnit)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param value A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity). (type = number)
#' @return A modified spec
#' @export
#' @md
#' @seealso [vl_encode_yError2()]
vl_YError2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_mark
#' 
#' Add a mark to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param align The horizontal alignment of the text. One of `"left"`, `"right"`, `"center"`. (type = Align)
#' @param angle The rotation angle of the text, in degrees. (type = number)
#' @param baseline The vertical alignment of the text. One of `"top"`, `"middle"`, `"bottom"`.
#' 
#' __Default value:__ `"middle"` (type = TextBaseline)
#' @param binSpacing Offset between bars for binned field.  Ideal value for this is either 0 (Preferred by statisticians) or 1 (Vega-Lite Default, D3 example style).
#' 
#' __Default value:__ `1` (type = number)
#' @param clip Whether a mark be clipped to the enclosing group’s width and height. (type = boolean)
#' @param color Default color.  Note that `fill` and `stroke` have higher precedence than `color` and will override `color`.
#' 
#' __Default value:__ <span style="color: #4682b4;">&#9632;</span> `"#4682b4"`
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config). (type = string)
#' @param cornerRadius The radius in pixels of rounded rectangle corners.
#' 
#' __Default value:__ `0` (type = number)
#' @param cursor The mouse cursor used over the mark. Any valid [CSS cursor type](https://developer.mozilla.org/en-US/docs/Web/CSS/cursor#Values) can be used. (type = Cursor)
#' @param dir The direction of the text. One of `"ltr"` (left-to-right) or `"rtl"` (right-to-left). This property determines on which side is truncated in response to the limit parameter.
#' 
#' __Default value:__ `"ltr"` (type = Dir)
#' @param dx The horizontal offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property. (type = number)
#' @param dy The vertical offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property. (type = number)
#' @param ellipsis The ellipsis string for text truncated in response to the limit parameter.
#' 
#' __Default value:__ `"…"` (type = string)
#' @param fill Default Fill Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None) (type = Color)
#' @param fillOpacity The fill opacity (value between [0,1]).
#' 
#' __Default value:__ `1` (type = number)
#' @param filled Whether the mark's color should be used as fill color instead of stroke color.
#' 
#' __Default value:__ `false` for `point`, `line` and `rule`; otherwise, `true`.
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config). (type = boolean)
#' @param font The typeface to set the text in (e.g., `"Helvetica Neue"`). (type = string)
#' @param fontSize The font size, in pixels. (type = number)
#' @param fontStyle The font style (e.g., `"italic"`). (type = FontStyle)
#' @param fontWeight The font weight.
#' This can be either a string (e.g `"bold"`, `"normal"`) or a number (`100`, `200`, `300`, ..., `900` where `"normal"` = `400` and `"bold"` = `700`). (type = FontWeight)
#' @param href A URL to load upon mouse click. If defined, the mark acts as a hyperlink. (type = string)
#' @param interpolate The line interpolation method to use for line and area marks. One of the following:
#' - `"linear"`: piecewise linear segments, as in a polyline.
#' - `"linear-closed"`: close the linear segments to form a polygon.
#' - `"step"`: alternate between horizontal and vertical segments, as in a step function.
#' - `"step-before"`: alternate between vertical and horizontal segments, as in a step function.
#' - `"step-after"`: alternate between horizontal and vertical segments, as in a step function.
#' - `"basis"`: a B-spline, with control point duplication on the ends.
#' - `"basis-open"`: an open B-spline; may not intersect the start or end.
#' - `"basis-closed"`: a closed B-spline, as in a loop.
#' - `"cardinal"`: a Cardinal spline, with control point duplication on the ends.
#' - `"cardinal-open"`: an open Cardinal spline; may not intersect the start or end, but will intersect other control points.
#' - `"cardinal-closed"`: a closed Cardinal spline, as in a loop.
#' - `"bundle"`: equivalent to basis, except the tension parameter is used to straighten the spline.
#' - `"monotone"`: cubic interpolation that preserves monotonicity in y. (type = Interpolate)
#' @param limit The maximum length of the text mark in pixels. The text value will be automatically truncated if the rendered size exceeds the limit.
#' 
#' __Default value:__ `0`, indicating no limit (type = number)
#' @param line A flag for overlaying line on top of area marks, or an object defining the properties of the overlayed lines.
#' 
#' - If this value is an empty object (`{}`) or `true`, lines with default properties will be used.
#' 
#' - If this value is `false`, no lines would be automatically added to area marks.
#' 
#' __Default value:__ `false`. (type = Varies)
#' @param opacity The overall opacity (value between [0,1]).
#' 
#' __Default value:__ `0.7` for non-aggregate plots with `point`, `tick`, `circle`, or `square` marks or layered `bar` charts and `1` otherwise. (type = number)
#' @param order For line and trail marks, this `order` property can be set to `null` or `false` to make the lines use the original order in the data sources. (type = null OR boolean)
#' @param orient The orientation of a non-stacked bar, tick, area, and line charts.
#' The value is either horizontal (default) or vertical.
#' - For bar, rule and tick, this determines whether the size of the bar and tick
#' should be applied to x or y dimension.
#' - For area, this property determines the orient property of the Vega output.
#' - For line and trail marks, this property determines the sort order of the points in the line
#' if `config.sortLineBy` is not specified.
#' For stacked charts, this is always determined by the orientation of the stack;
#' therefore explicitly specified value will be ignored. (type = Orientation)
#' @param point A flag for overlaying points on top of line or area marks, or an object defining the properties of the overlayed points.
#' 
#' - If this property is `"transparent"`, transparent points will be used (for enhancing tooltips and selections).
#' 
#' - If this property is an empty object (`{}`) or `true`, filled points with default properties will be used.
#' 
#' - If this property is `false`, no points would be automatically added to line or area marks.
#' 
#' __Default value:__ `false`. (type = Varies)
#' @param radius Polar coordinate radial offset, in pixels, of the text label from the origin determined by the `x` and `y` properties. (type = number)
#' @param shape Shape of the point marks. Supported values include:
#' - plotting shapes: `"circle"`, `"square"`, `"cross"`, `"diamond"`, `"triangle-up"`, `"triangle-down"`, `"triangle-right"`, or `"triangle-left"`.
#' - the line symbol `"stroke"`
#' - centered directional shapes `"arrow"`, `"wedge"`, or `"triangle"`
#' - a custom [SVG path string](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) (For correct sizing, custom shape paths should be defined within a square bounding box with coordinates ranging from -1 to 1 along both the x and y dimensions.)
#' 
#' __Default value:__ `"circle"` (type = string)
#' @param size Default size for marks.
#' - For `point`/`circle`/`square`, this represents the pixel area of the marks. For example: in the case of circles, the radius is determined in part by the square root of the size value.
#' - For `bar`, this represents the band size of the bar, in pixels.
#' - For `text`, this represents the font size, in pixels.
#' 
#' __Default value:__ `30` for point, circle, square marks; `rangeStep` - 1 for bar marks with discrete dimensions; `5` for bar marks with continuous dimensions; `11` for text marks. (type = number)
#' @param stroke Default Stroke Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None) (type = Color)
#' @param strokeCap The stroke cap for line ending style. One of `"butt"`, `"round"`, or `"square"`.
#' 
#' __Default value:__ `"square"` (type = StrokeCap)
#' @param strokeDash An array of alternating stroke, space lengths for creating dashed or dotted lines. (type = array)
#' @param strokeDashOffset The offset (in pixels) into which to begin drawing with the stroke dash array. (type = number)
#' @param strokeJoin The stroke line join method. One of `"miter"`, `"round"` or `"bevel"`.
#' 
#' __Default value:__ `"miter"` (type = StrokeJoin)
#' @param strokeMiterLimit The miter limit at which to bevel a line join. (type = number)
#' @param strokeOpacity The stroke opacity (value between [0,1]).
#' 
#' __Default value:__ `1` (type = number)
#' @param strokeWidth The stroke width, in pixels. (type = number)
#' @param style A string or array of strings indicating the name of custom styles to apply to the mark. A style is a named collection of mark property defaults defined within the [style configuration](https://vega.github.io/vega-lite/docs/mark.html#style-config). If style is an array, later styles will override earlier styles. Any [mark properties](https://vega.github.io/vega-lite/docs/encoding.html#mark-prop) explicitly defined within the `encoding` will override a style default.
#' 
#' __Default value:__ The mark's name.  For example, a bar mark will have style `"bar"` by default.
#' __Note:__ Any specified style will augment the default style. For example, a bar mark with `"style": "foo"` will receive from `config.style.bar` and `config.style.foo` (the specified style `"foo"` has higher precedence). (type = Varies)
#' @param tension Depending on the interpolation type, sets the tension parameter (for line and area marks). (type = number)
#' @param text Placeholder text if the `text` channel is not specified (type = string)
#' @param theta Polar coordinate angle, in radians, of the text label from the origin determined by the `x` and `y` properties. Values for `theta` follow the same convention of `arc` mark `startAngle` and `endAngle` properties: angles are measured in radians, with `0` indicating "north". (type = number)
#' @param thickness Thickness of the tick mark.
#' 
#' __Default value:__  `1` (type = number)
#' @param tooltip The tooltip text string to show upon mouse hover or an object defining which fields should the tooltip be derived from.
#' 
#' - If `tooltip` is `{"content": "encoding"}`, then all fields from `encoding` will be used.
#' - If `tooltip` is `{"content": "data"}`, then all fields that appear in the highlighted data point will be used.
#' - If set to `null`, then no tooltip will be used. (type = Varies)
#' @param type The mark type. This could a primitive mark type
#' (one of `"bar"`, `"circle"`, `"square"`, `"tick"`, `"line"`,
#' `"area"`, `"point"`, `"geoshape"`, `"rule"`, and `"text"`)
#' or a composite mark type (`"boxplot"`, `"errorband"`, `"errorbar"`). (type = Mark)
#' @param x X coordinates of the marks, or width of horizontal `"bar"` and `"area"` without `x2`. (type = number)
#' @param x2 X2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`. (type = number)
#' @param x2Offset Offset for x2-position. (type = number)
#' @param xOffset Offset for x-position. (type = number)
#' @param y Y coordinates of the marks, or height of vertical `"bar"` and `"area"` without `y2` (type = number)
#' @param y2 Y2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`. (type = number)
#' @param y2Offset Offset for y2-position. (type = number)
#' @param yOffset Offset for y-position. (type = number)
#' @return A modified spec
#' @export
#' @name vl_mark
vl_mark <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  rlang::exec(.add_mark, !!!args_out)
}
 
#' @name vl_mark
#' @export
vl_mark_boxplot <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.mark = 'boxplot'))
  rlang::exec(.add_mark, !!!args_out)
}
 
#' @name vl_mark
#' @export
vl_mark_errorbar <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.mark = 'errorbar'))
  rlang::exec(.add_mark, !!!args_out)
}
 
#' @name vl_mark
#' @export
vl_mark_errorband <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.mark = 'errorband'))
  rlang::exec(.add_mark, !!!args_out)
}
 
#' @name vl_mark
#' @export
vl_mark_area <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.mark = 'area'))
  rlang::exec(.add_mark, !!!args_out)
}
 
#' @name vl_mark
#' @export
vl_mark_bar <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.mark = 'bar'))
  rlang::exec(.add_mark, !!!args_out)
}
 
#' @name vl_mark
#' @export
vl_mark_line <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.mark = 'line'))
  rlang::exec(.add_mark, !!!args_out)
}
 
#' @name vl_mark
#' @export
vl_mark_trail <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.mark = 'trail'))
  rlang::exec(.add_mark, !!!args_out)
}
 
#' @name vl_mark
#' @export
vl_mark_point <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.mark = 'point'))
  rlang::exec(.add_mark, !!!args_out)
}
 
#' @name vl_mark
#' @export
vl_mark_text <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.mark = 'text'))
  rlang::exec(.add_mark, !!!args_out)
}
 
#' @name vl_mark
#' @export
vl_mark_tick <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.mark = 'tick'))
  rlang::exec(.add_mark, !!!args_out)
}
 
#' @name vl_mark
#' @export
vl_mark_rect <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.mark = 'rect'))
  rlang::exec(.add_mark, !!!args_out)
}
 
#' @name vl_mark
#' @export
vl_mark_rule <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.mark = 'rule'))
  rlang::exec(.add_mark, !!!args_out)
}
 
#' @name vl_mark
#' @export
vl_mark_circle <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.mark = 'circle'))
  rlang::exec(.add_mark, !!!args_out)
}
 
#' @name vl_mark
#' @export
vl_mark_square <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.mark = 'square'))
  rlang::exec(.add_mark, !!!args_out)
}
 
#' @name vl_mark
#' @export
vl_mark_geoshape <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.mark = 'geoshape'))
  rlang::exec(.add_mark, !!!args_out)
}
 
#' vl_add_data
#' 
#' Add data to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param format An object that specifies the format for parsing the data. (type = DataFormat)
#' @param name Provide a placeholder name and bind data at runtime. (type = string)
#' @param url An URL from which to load the data set. Use the `format.type` property
#' to ensure the loaded data is correctly parsed. (type = string)
#' @param values The full data set, included inline. This can be an array of objects or primitive values, an object, or a string.
#' Arrays of primitive values are ingested as objects with a `data` property. Strings are parsed according to the specified format type. (type = InlineDataset)
#' @return A modified spec
#' @export
#' @name add_data
vl_add_data <- function(spec, format = NULL, name = NULL, url = NULL, values = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  rlang::exec(.add_data, !!!args_out)
}
 
#' vl_transform_filter
#' 
#' Add FilterTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param filter The `filter` property must be one of the predicate definitions:
#' 
#' 1) an [expression](https://vega.github.io/vega-lite/docs/types.html#expression) string,
#' where `datum` can be used to refer to the current data object
#' 
#' 2) one of the field predicates: [`equal`](https://vega.github.io/vega-lite/docs/filter.html#equal-predicate),
#' [`lt`](https://vega.github.io/vega-lite/docs/filter.html#lt-predicate),
#' [`lte`](https://vega.github.io/vega-lite/docs/filter.html#lte-predicate),
#' [`gt`](https://vega.github.io/vega-lite/docs/filter.html#gt-predicate),
#' [`gte`](https://vega.github.io/vega-lite/docs/filter.html#gte-predicate),
#' [`range`](https://vega.github.io/vega-lite/docs/filter.html#range-predicate),
#' [`oneOf`](https://vega.github.io/vega-lite/docs/filter.html#one-of-predicate),
#' or [`valid`](https://vega.github.io/vega-lite/docs/filter.html#valid-predicate),
#' 
#' 3) a [selection predicate](https://vega.github.io/vega-lite/docs/filter.html#selection-predicate)
#' 
#' 4) a logical operand that combines (1), (2), or (3). (type = LogicalOperand<Predicate>)
#' @return A modified spec
#' @export
vl_transform_filter <- function(spec, filter = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'filter'))
  rlang::exec(.add_transform, !!!args_out)
}
 
#' vl_transform_calculate
#' 
#' Add CalculateTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param as The field for storing the computed formula value. (type = string)
#' @param calculate A [expression](https://vega.github.io/vega-lite/docs/types.html#expression) string. Use the variable `datum` to refer to the current data object. (type = string)
#' @return A modified spec
#' @export
vl_transform_calculate <- function(spec, as = NULL, calculate = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'calculate'))
  rlang::exec(.add_transform, !!!args_out)
}
 
#' vl_transform_lookup
#' 
#' Add LookupTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param as The field or fields for storing the computed formula value.
#' If `from.fields` is specified, the transform will use the same names for `as`.
#' If `from.fields` is not specified, `as` has to be a string and we put the whole object into the data under the specified name. (type = Varies)
#' @param default The default value to use if lookup fails.
#' 
#' __Default value:__ `null` (type = string)
#' @param from Secondary data reference. (type = LookupData)
#' @param lookup Key in primary data source. (type = string)
#' @return A modified spec
#' @export
vl_transform_lookup <- function(spec, as = NULL, default = NULL, from = NULL, lookup = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'lookup'))
  rlang::exec(.add_transform, !!!args_out)
}
 
#' vl_transform_bin
#' 
#' Add BinTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param as The output fields at which to write the start and end bin values. (type = Varies)
#' @param bin An object indicating bin properties, or simply `true` for using default bin parameters. (type = Varies)
#' @param field The data field to bin. (type = string)
#' @return A modified spec
#' @export
vl_transform_bin <- function(spec, as = NULL, bin = NULL, field = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'bin'))
  rlang::exec(.add_transform, !!!args_out)
}
 
#' vl_transform_timeunit
#' 
#' Add TimeUnitTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param as The output field to write the timeUnit value. (type = string)
#' @param field The data field to apply time unit. (type = string)
#' @param timeUnit The timeUnit. (type = TimeUnit)
#' @return A modified spec
#' @export
vl_transform_timeunit <- function(spec, as = NULL, field = NULL, timeUnit = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'timeunit'))
  rlang::exec(.add_transform, !!!args_out)
}
 
#' vl_transform_impute
#' 
#' Add ImputeTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param frame A frame specification as a two-element array used to control the window over which the specified method is applied. The array entries should either be a number indicating the offset from the current data object, or null to indicate unbounded rows preceding or following the current data object.  For example, the value `[-5, 5]` indicates that the window should include five objects preceding and five objects following the current object.
#' 
#' __Default value:__:  `[null, null]` indicating that the window includes all objects. (type = array)
#' @param groupby An optional array of fields by which to group the values.
#' Imputation will then be performed on a per-group basis. (type = array)
#' @param impute The data field for which the missing values should be imputed. (type = string)
#' @param key A key field that uniquely identifies data objects within a group.
#' Missing key values (those occurring in the data but not in the current group) will be imputed. (type = string)
#' @param keyvals Defines the key values that should be considered for imputation.
#' An array of key values or an object defining a [number sequence](https://vega.github.io/vega-lite/docs/impute.html#sequence-def).
#' 
#' If provided, this will be used in addition to the key values observed within the input data.  If not provided, the values will be derived from all unique values of the `key` field. For `impute` in `encoding`, the key field is the x-field if the y-field is imputed, or vice versa.
#' 
#' If there is no impute grouping, this property _must_ be specified. (type = Varies)
#' @param method The imputation method to use for the field value of imputed data objects.
#' One of `value`, `mean`, `median`, `max` or `min`.
#' 
#' __Default value:__  `"value"` (type = ImputeMethod)
#' @param value The field value to use when the imputation `method` is `"value"`. (type = Varies)
#' @return A modified spec
#' @export
vl_transform_impute <- function(spec, frame = NULL, groupby = NULL, impute = NULL, key = NULL, keyvals = NULL, method = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'impute'))
  rlang::exec(.add_transform, !!!args_out)
}
 
#' vl_transform_aggregate
#' 
#' Add AggregateTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Array of objects that define fields to aggregate. (type = array)
#' @param groupby The data fields to group by. If not specified, a single group containing all data objects will be used. (type = array)
#' @return A modified spec
#' @export
vl_transform_aggregate <- function(spec, aggregate = NULL, groupby = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'aggregate'))
  rlang::exec(.add_transform, !!!args_out)
}
 
#' vl_transform_window
#' 
#' Add WindowTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param frame A frame specification as a two-element array indicating how the sliding window should proceed. The array entries should either be a number indicating the offset from the current data object, or null to indicate unbounded rows preceding or following the current data object. The default value is `[null, 0]`, indicating that the sliding window includes the current object and all preceding objects. The value `[-5, 5]` indicates that the window should include five objects preceding and five objects following the current object. Finally, `[null, null]` indicates that the window frame should always include all data objects. If you this frame and want to assign the same value to add objects, you can use the simpler [join aggregate transform](https://vega.github.io/vega-lite/docs/joinaggregate.html). The only operators affected are the aggregation operations and the `first_value`, `last_value`, and `nth_value` window operations. The other window operations are not affected by this.
#' 
#' __Default value:__:  `[null, 0]` (includes the current object and all preceding objects) (type = array)
#' @param groupby The data fields for partitioning the data objects into separate windows. If unspecified, all data points will be in a single window. (type = array)
#' @param ignorePeers Indicates if the sliding window frame should ignore peer values (data that are considered identical by the sort criteria). The default is false, causing the window frame to expand to include all peer values. If set to true, the window frame will be defined by offset values only. This setting only affects those operations that depend on the window frame, namely aggregation operations and the first_value, last_value, and nth_value window operations.
#' 
#' __Default value:__ `false` (type = boolean)
#' @param sort A sort field definition for sorting data objects within a window. If two data objects are considered equal by the comparator, they are considered “peer” values of equal rank. If sort is not specified, the order is undefined: data objects are processed in the order they are observed and none are considered peers (the ignorePeers parameter is ignored and treated as if set to `true`). (type = array)
#' @param window The definition of the fields in the window, and what calculations to use. (type = array)
#' @return A modified spec
#' @export
vl_transform_window <- function(spec, frame = NULL, groupby = NULL, ignorePeers = NULL, sort = NULL, window = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'window'))
  rlang::exec(.add_transform, !!!args_out)
}
 
#' vl_transform_joinaggregate
#' 
#' Add JoinAggregateTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param groupby The data fields for partitioning the data objects into separate groups. If unspecified, all data points will be in a single group. (type = array)
#' @param joinaggregate The definition of the fields in the join aggregate, and what calculations to use. (type = array)
#' @return A modified spec
#' @export
vl_transform_joinaggregate <- function(spec, groupby = NULL, joinaggregate = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'joinaggregate'))
  rlang::exec(.add_transform, !!!args_out)
}
 
#' vl_transform_stack
#' 
#' Add StackTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param as Output field names. This can be either a string or an array of strings with
#' two elements denoting the name for the fields for stack start and stack end
#' respectively.
#' If a single string(eg."val") is provided, the end field will be "val_end". (type = Varies)
#' @param groupby The data fields to group by. (type = array)
#' @param offset Mode for stacking marks.
#' __Default value:__ `"zero"` (type = string)
#' @param sort Field that determines the order of leaves in the stacked charts. (type = array)
#' @param stack The field which is stacked. (type = string)
#' @return A modified spec
#' @export
vl_transform_stack <- function(spec, as = NULL, groupby = NULL, offset = NULL, sort = NULL, stack = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'stack'))
  rlang::exec(.add_transform, !!!args_out)
}
 
#' vl_transform_flatten
#' 
#' Add FlattenTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param as The output field names for extracted array values.
#' 
#' __Default value:__ The field name of the corresponding array field (type = array)
#' @param flatten An array of one or more data fields containing arrays to flatten.
#' If multiple fields are specified, their array values should have a parallel structure, ideally with the same length.
#' If the lengths of parallel arrays do not match,
#' the longest array will be used with `null` values added for missing entries. (type = array)
#' @return A modified spec
#' @export
vl_transform_flatten <- function(spec, as = NULL, flatten = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'flatten'))
  rlang::exec(.add_transform, !!!args_out)
}
 
#' vl_transform_fold
#' 
#' Add FoldTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param as The output field names for the key and value properties produced by the fold transform.
#' __Default value:__ `["key", "value"]` (type = array)
#' @param fold An array of data fields indicating the properties to fold. (type = array)
#' @return A modified spec
#' @export
vl_transform_fold <- function(spec, as = NULL, fold = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'fold'))
  rlang::exec(.add_transform, !!!args_out)
}
 
#' vl_transform_sample
#' 
#' Add SampleTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param sample The maximum number of data objects to include in the sample.
#' 
#' __Default value:__ `1000` (type = number)
#' @return A modified spec
#' @export
vl_transform_sample <- function(spec, sample = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'sample'))
  rlang::exec(.add_transform, !!!args_out)
}
 
#' vl_FilterTransform
#' 
#' Create spec for FilterTransform.
#' @param filter The `filter` property must be one of the predicate definitions:
#' 
#' 1) an [expression](https://vega.github.io/vega-lite/docs/types.html#expression) string,
#' where `datum` can be used to refer to the current data object
#' 
#' 2) one of the field predicates: [`equal`](https://vega.github.io/vega-lite/docs/filter.html#equal-predicate),
#' [`lt`](https://vega.github.io/vega-lite/docs/filter.html#lt-predicate),
#' [`lte`](https://vega.github.io/vega-lite/docs/filter.html#lte-predicate),
#' [`gt`](https://vega.github.io/vega-lite/docs/filter.html#gt-predicate),
#' [`gte`](https://vega.github.io/vega-lite/docs/filter.html#gte-predicate),
#' [`range`](https://vega.github.io/vega-lite/docs/filter.html#range-predicate),
#' [`oneOf`](https://vega.github.io/vega-lite/docs/filter.html#one-of-predicate),
#' or [`valid`](https://vega.github.io/vega-lite/docs/filter.html#valid-predicate),
#' 
#' 3) a [selection predicate](https://vega.github.io/vega-lite/docs/filter.html#selection-predicate)
#' 
#' 4) a logical operand that combines (1), (2), or (3). (type = LogicalOperand<Predicate>)
#' @return A modified spec
#' @export
#' @md
vl_FilterTransform <- function(filter = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_CalculateTransform
#' 
#' Create spec for CalculateTransform.
#' @param as The field for storing the computed formula value. (type = string)
#' @param calculate A [expression](https://vega.github.io/vega-lite/docs/types.html#expression) string. Use the variable `datum` to refer to the current data object. (type = string)
#' @return A modified spec
#' @export
#' @md
vl_CalculateTransform <- function(as = NULL, calculate = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_LookupTransform
#' 
#' Create spec for LookupTransform.
#' @param as The field or fields for storing the computed formula value.
#' If `from.fields` is specified, the transform will use the same names for `as`.
#' If `from.fields` is not specified, `as` has to be a string and we put the whole object into the data under the specified name. (type = Varies)
#' @param default The default value to use if lookup fails.
#' 
#' __Default value:__ `null` (type = string)
#' @param from Secondary data reference. (type = LookupData)
#' @param lookup Key in primary data source. (type = string)
#' @return A modified spec
#' @export
#' @md
vl_LookupTransform <- function(as = NULL, default = NULL, from = NULL, lookup = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_BinTransform
#' 
#' Create spec for BinTransform.
#' @param as The output fields at which to write the start and end bin values. (type = Varies)
#' @param bin An object indicating bin properties, or simply `true` for using default bin parameters. (type = Varies)
#' @param field The data field to bin. (type = string)
#' @return A modified spec
#' @export
#' @md
vl_BinTransform <- function(as = NULL, bin = NULL, field = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_TimeUnitTransform
#' 
#' Create spec for TimeUnitTransform.
#' @param as The output field to write the timeUnit value. (type = string)
#' @param field The data field to apply time unit. (type = string)
#' @param timeUnit The timeUnit. (type = TimeUnit)
#' @return A modified spec
#' @export
#' @md
vl_TimeUnitTransform <- function(as = NULL, field = NULL, timeUnit = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_ImputeTransform
#' 
#' Create spec for ImputeTransform.
#' @param frame A frame specification as a two-element array used to control the window over which the specified method is applied. The array entries should either be a number indicating the offset from the current data object, or null to indicate unbounded rows preceding or following the current data object.  For example, the value `[-5, 5]` indicates that the window should include five objects preceding and five objects following the current object.
#' 
#' __Default value:__:  `[null, null]` indicating that the window includes all objects. (type = array)
#' @param groupby An optional array of fields by which to group the values.
#' Imputation will then be performed on a per-group basis. (type = array)
#' @param impute The data field for which the missing values should be imputed. (type = string)
#' @param key A key field that uniquely identifies data objects within a group.
#' Missing key values (those occurring in the data but not in the current group) will be imputed. (type = string)
#' @param keyvals Defines the key values that should be considered for imputation.
#' An array of key values or an object defining a [number sequence](https://vega.github.io/vega-lite/docs/impute.html#sequence-def).
#' 
#' If provided, this will be used in addition to the key values observed within the input data.  If not provided, the values will be derived from all unique values of the `key` field. For `impute` in `encoding`, the key field is the x-field if the y-field is imputed, or vice versa.
#' 
#' If there is no impute grouping, this property _must_ be specified. (type = Varies)
#' @param method The imputation method to use for the field value of imputed data objects.
#' One of `value`, `mean`, `median`, `max` or `min`.
#' 
#' __Default value:__  `"value"` (type = ImputeMethod)
#' @param value The field value to use when the imputation `method` is `"value"`. (type = Varies)
#' @return A modified spec
#' @export
#' @md
vl_ImputeTransform <- function(frame = NULL, groupby = NULL, impute = NULL, key = NULL, keyvals = NULL, method = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_AggregateTransform
#' 
#' Create spec for AggregateTransform.
#' @param aggregate Array of objects that define fields to aggregate. (type = array)
#' @param groupby The data fields to group by. If not specified, a single group containing all data objects will be used. (type = array)
#' @return A modified spec
#' @export
#' @md
vl_AggregateTransform <- function(aggregate = NULL, groupby = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_WindowTransform
#' 
#' Create spec for WindowTransform.
#' @param frame A frame specification as a two-element array indicating how the sliding window should proceed. The array entries should either be a number indicating the offset from the current data object, or null to indicate unbounded rows preceding or following the current data object. The default value is `[null, 0]`, indicating that the sliding window includes the current object and all preceding objects. The value `[-5, 5]` indicates that the window should include five objects preceding and five objects following the current object. Finally, `[null, null]` indicates that the window frame should always include all data objects. If you this frame and want to assign the same value to add objects, you can use the simpler [join aggregate transform](https://vega.github.io/vega-lite/docs/joinaggregate.html). The only operators affected are the aggregation operations and the `first_value`, `last_value`, and `nth_value` window operations. The other window operations are not affected by this.
#' 
#' __Default value:__:  `[null, 0]` (includes the current object and all preceding objects) (type = array)
#' @param groupby The data fields for partitioning the data objects into separate windows. If unspecified, all data points will be in a single window. (type = array)
#' @param ignorePeers Indicates if the sliding window frame should ignore peer values (data that are considered identical by the sort criteria). The default is false, causing the window frame to expand to include all peer values. If set to true, the window frame will be defined by offset values only. This setting only affects those operations that depend on the window frame, namely aggregation operations and the first_value, last_value, and nth_value window operations.
#' 
#' __Default value:__ `false` (type = boolean)
#' @param sort A sort field definition for sorting data objects within a window. If two data objects are considered equal by the comparator, they are considered “peer” values of equal rank. If sort is not specified, the order is undefined: data objects are processed in the order they are observed and none are considered peers (the ignorePeers parameter is ignored and treated as if set to `true`). (type = array)
#' @param window The definition of the fields in the window, and what calculations to use. (type = array)
#' @return A modified spec
#' @export
#' @md
vl_WindowTransform <- function(frame = NULL, groupby = NULL, ignorePeers = NULL, sort = NULL, window = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_JoinAggregateTransform
#' 
#' Create spec for JoinAggregateTransform.
#' @param groupby The data fields for partitioning the data objects into separate groups. If unspecified, all data points will be in a single group. (type = array)
#' @param joinaggregate The definition of the fields in the join aggregate, and what calculations to use. (type = array)
#' @return A modified spec
#' @export
#' @md
vl_JoinAggregateTransform <- function(groupby = NULL, joinaggregate = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_StackTransform
#' 
#' Create spec for StackTransform.
#' @param as Output field names. This can be either a string or an array of strings with
#' two elements denoting the name for the fields for stack start and stack end
#' respectively.
#' If a single string(eg."val") is provided, the end field will be "val_end". (type = Varies)
#' @param groupby The data fields to group by. (type = array)
#' @param offset Mode for stacking marks.
#' __Default value:__ `"zero"` (type = string)
#' @param sort Field that determines the order of leaves in the stacked charts. (type = array)
#' @param stack The field which is stacked. (type = string)
#' @return A modified spec
#' @export
#' @md
vl_StackTransform <- function(as = NULL, groupby = NULL, offset = NULL, sort = NULL, stack = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_FlattenTransform
#' 
#' Create spec for FlattenTransform.
#' @param as The output field names for extracted array values.
#' 
#' __Default value:__ The field name of the corresponding array field (type = array)
#' @param flatten An array of one or more data fields containing arrays to flatten.
#' If multiple fields are specified, their array values should have a parallel structure, ideally with the same length.
#' If the lengths of parallel arrays do not match,
#' the longest array will be used with `null` values added for missing entries. (type = array)
#' @return A modified spec
#' @export
#' @md
vl_FlattenTransform <- function(as = NULL, flatten = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_FoldTransform
#' 
#' Create spec for FoldTransform.
#' @param as The output field names for the key and value properties produced by the fold transform.
#' __Default value:__ `["key", "value"]` (type = array)
#' @param fold An array of data fields indicating the properties to fold. (type = array)
#' @return A modified spec
#' @export
#' @md
vl_FoldTransform <- function(as = NULL, fold = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_SampleTransform
#' 
#' Create spec for SampleTransform.
#' @param sample The maximum number of data objects to include in the sample.
#' 
#' __Default value:__ `1000` (type = number)
#' @return A modified spec
#' @export
#' @md
vl_SampleTransform <- function(sample = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
