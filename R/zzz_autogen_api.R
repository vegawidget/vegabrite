
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
 
#' vl_color
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
#' @md
#' @seealso [vl_encode_color()]
vl_color <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_detail
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
#' @md
#' @seealso [vl_encode_detail()]
vl_detail <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_fill
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
#' @md
#' @seealso [vl_encode_fill()]
vl_fill <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_fillOpacity
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
#' @md
#' @seealso [vl_encode_fillOpacity()]
vl_fillOpacity <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_href
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
#' @md
#' @seealso [vl_encode_href()]
vl_href <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_key
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
#' @md
#' @seealso [vl_encode_key()]
vl_key <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_latitude
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
#' @md
#' @seealso [vl_encode_latitude()]
vl_latitude <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_latitude2
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
#' @md
#' @seealso [vl_encode_latitude2()]
vl_latitude2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_longitude
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
#' @md
#' @seealso [vl_encode_longitude()]
vl_longitude <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_longitude2
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
#' @md
#' @seealso [vl_encode_longitude2()]
vl_longitude2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_opacity
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
#' @md
#' @seealso [vl_encode_opacity()]
vl_opacity <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_order
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
#' @md
#' @seealso [vl_encode_order()]
vl_order <- function(aggregate = NULL, bin = NULL, field = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_shape
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
#' @md
#' @seealso [vl_encode_shape()]
vl_shape <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_size
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
#' @md
#' @seealso [vl_encode_size()]
vl_size <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_stroke
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
#' @md
#' @seealso [vl_encode_stroke()]
vl_stroke <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_strokeOpacity
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
#' @md
#' @seealso [vl_encode_strokeOpacity()]
vl_strokeOpacity <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_strokeWidth
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
#' @md
#' @seealso [vl_encode_strokeWidth()]
vl_strokeWidth <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_text
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
#' @md
#' @seealso [vl_encode_text()]
vl_text <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, format = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_tooltip
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
#' @md
#' @seealso [vl_encode_tooltip()]
vl_tooltip <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, format = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_x
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
#' @md
#' @seealso [vl_encode_x()]
vl_x <- function(aggregate = NULL, axis = NULL, bin = NULL, field = NULL, impute = NULL, scale = NULL, sort = NULL, stack = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_x2
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
#' @md
#' @seealso [vl_encode_x2()]
vl_x2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_xError
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
#' @md
#' @seealso [vl_encode_xError()]
vl_xError <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_xError2
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
#' @md
#' @seealso [vl_encode_xError2()]
vl_xError2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_y
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
#' @md
#' @seealso [vl_encode_y()]
vl_y <- function(aggregate = NULL, axis = NULL, bin = NULL, field = NULL, impute = NULL, scale = NULL, sort = NULL, stack = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_y2
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
#' @md
#' @seealso [vl_encode_y2()]
vl_y2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_yError
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
#' @md
#' @seealso [vl_encode_yError()]
vl_yError <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 
#' vl_yError2
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
#' @md
#' @seealso [vl_encode_yError2()]
vl_yError2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
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
