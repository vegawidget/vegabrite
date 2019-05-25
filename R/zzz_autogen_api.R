#' vl_encode_color
#' 
#' Add encoding for color to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_detail
#' 
#' Add encoding for detail to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_fill
#' 
#' Add encoding for fill to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_fillOpacity
#' 
#' Add encoding for fillOpacity to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_href
#' 
#' Add encoding for href to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_key
#' 
#' Add encoding for key to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_latitude
#' 
#' Add encoding for latitude to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_latitude2
#' 
#' Add encoding for latitude2 to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_longitude
#' 
#' Add encoding for longitude to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_longitude2
#' 
#' Add encoding for longitude2 to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_opacity
#' 
#' Add encoding for opacity to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_order
#' 
#' Add encoding for order to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_shape
#' 
#' Add encoding for shape to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_size
#' 
#' Add encoding for size to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_stroke
#' 
#' Add encoding for stroke to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_strokeOpacity
#' 
#' Add encoding for strokeOpacity to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_strokeWidth
#' 
#' Add encoding for strokeWidth to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_text
#' 
#' Add encoding for text to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_tooltip
#' 
#' Add encoding for tooltip to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_x
#' 
#' Add encoding for x to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_x2
#' 
#' Add encoding for x2 to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_xError
#' 
#' Add encoding for xError to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_xError2
#' 
#' Add encoding for xError2 to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_y
#' 
#' Add encoding for y to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_y2
#' 
#' Add encoding for y2 to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_yError
#' 
#' Add encoding for yError to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_encode_yError2
#' 
#' Add encoding for yError2 to a vega-lite spec.
#' @param spec A vega-lite spec.
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
} #' vl_Color
#' 
#' Create spec for Color.
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
vl_Color <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_Detail
#' 
#' Create spec for Detail.
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
vl_Detail <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_Fill
#' 
#' Create spec for Fill.
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
vl_Fill <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_FillOpacity
#' 
#' Create spec for FillOpacity.
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
vl_FillOpacity <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_Href
#' 
#' Create spec for Href.
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
vl_Href <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_Key
#' 
#' Create spec for Key.
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
vl_Key <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_Latitude
#' 
#' Create spec for Latitude.
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
vl_Latitude <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_Latitude2
#' 
#' Create spec for Latitude2.
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
vl_Latitude2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_Longitude
#' 
#' Create spec for Longitude.
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
vl_Longitude <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_Longitude2
#' 
#' Create spec for Longitude2.
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
vl_Longitude2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_Opacity
#' 
#' Create spec for Opacity.
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
vl_Opacity <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_Order
#' 
#' Create spec for Order.
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
vl_Order <- function(aggregate = NULL, bin = NULL, field = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_Shape
#' 
#' Create spec for Shape.
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
vl_Shape <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_Size
#' 
#' Create spec for Size.
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
vl_Size <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_Stroke
#' 
#' Create spec for Stroke.
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
vl_Stroke <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_StrokeOpacity
#' 
#' Create spec for StrokeOpacity.
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
vl_StrokeOpacity <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_StrokeWidth
#' 
#' Create spec for StrokeWidth.
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
vl_StrokeWidth <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_Text
#' 
#' Create spec for Text.
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
vl_Text <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, format = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_Tooltip
#' 
#' Create spec for Tooltip.
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
vl_Tooltip <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, format = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_X
#' 
#' Create spec for X.
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
vl_X <- function(aggregate = NULL, axis = NULL, bin = NULL, field = NULL, impute = NULL, scale = NULL, sort = NULL, stack = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_X2
#' 
#' Create spec for X2.
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
vl_X2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_XError
#' 
#' Create spec for XError.
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
vl_XError <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_XError2
#' 
#' Create spec for XError2.
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
vl_XError2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_Y
#' 
#' Create spec for Y.
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
vl_Y <- function(aggregate = NULL, axis = NULL, bin = NULL, field = NULL, impute = NULL, scale = NULL, sort = NULL, stack = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_Y2
#' 
#' Create spec for Y2.
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
vl_Y2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_YError
#' 
#' Create spec for YError.
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
vl_YError <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_YError2
#' 
#' Create spec for YError2.
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
vl_YError2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_bin_color
#' 
#' Add bin transform to color encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_color <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'color'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_detail
#' 
#' Add bin transform to detail encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_detail <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'detail'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_fill
#' 
#' Add bin transform to fill encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_fill <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'fill'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_fillOpacity
#' 
#' Add bin transform to fillOpacity encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_fillOpacity <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'fillOpacity'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_href
#' 
#' Add bin transform to href encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_href <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'href'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_key
#' 
#' Add bin transform to key encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_key <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'key'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_latitude
#' 
#' Add bin transform to latitude encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_latitude <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'latitude'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_latitude2
#' 
#' Add bin transform to latitude2 encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_latitude2 <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'latitude2'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_longitude
#' 
#' Add bin transform to longitude encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_longitude <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'longitude'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_longitude2
#' 
#' Add bin transform to longitude2 encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_longitude2 <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'longitude2'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_opacity
#' 
#' Add bin transform to opacity encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_opacity <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'opacity'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_order
#' 
#' Add bin transform to order encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_order <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'order'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_shape
#' 
#' Add bin transform to shape encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_shape <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'shape'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_size
#' 
#' Add bin transform to size encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_size <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'size'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_stroke
#' 
#' Add bin transform to stroke encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_stroke <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'stroke'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_strokeOpacity
#' 
#' Add bin transform to strokeOpacity encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_strokeOpacity <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'strokeOpacity'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_strokeWidth
#' 
#' Add bin transform to strokeWidth encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_strokeWidth <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'strokeWidth'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_text
#' 
#' Add bin transform to text encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_text <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'text'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_tooltip
#' 
#' Add bin transform to tooltip encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_tooltip <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'tooltip'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_x
#' 
#' Add bin transform to x encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_x <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'x'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_x2
#' 
#' Add bin transform to x2 encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_x2 <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'x2'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_xError
#' 
#' Add bin transform to xError encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_xError <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'xError'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_xError2
#' 
#' Add bin transform to xError2 encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_xError2 <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'xError2'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_y
#' 
#' Add bin transform to y encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_y <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'y'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_y2
#' 
#' Add bin transform to y2 encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_y2 <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'y2'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_yError
#' 
#' Add bin transform to yError encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_yError <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'yError'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_bin_yError2
#' 
#' Add bin transform to yError2 encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param anchor A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value.
#' 
#' __Default Value:__ the minimum bin extent value (type = number)
#' @param base The number base to use for automatic bin determination (default is base 10).
#' 
#' __Default value:__ `10` (type = number)
#' @param binned When set to true, Vega-Lite treats the input data as already binned. (type = boolean)
#' @param divide Scale factors indicating allowable subdivisions. The default value is [5, 2], which indicates that for base 10 numbers (the default base), the method may consider dividing bin sizes by 5 and/or 2. For example, for an initial step size of 10, the method can check if bin sizes of 2 (= 10/5), 5 (= 10/2), or 1 (= 10/(5*2)) might also satisfy the given constraints.
#' 
#' __Default value:__ `[5, 2]` (type = array)
#' @param extent A two-element (`[min, max]`) array indicating the range of desired bin values. (type = array)
#' @param maxbins Maximum number of bins.
#' 
#' __Default value:__ `6` for `row`, `column` and `shape` channels; `10` for other channels (type = number)
#' @param minstep A minimum allowable step size (particularly useful for integer values). (type = number)
#' @param nice If true (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten. (type = boolean)
#' @param step An exact step size to use between bins.
#' 
#' __Note:__ If provided, options such as maxbins will be ignored. (type = number)
#' @param steps An array of allowable step sizes to choose from. (type = array)
#' @return A modified spec
#' @export
vl_bin_yError2 <- function(spec, anchor = NULL, base = NULL, binned = NULL, divide = NULL, extent = NULL, maxbins = NULL, minstep = NULL, nice = NULL, step = NULL, steps = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'yError2'))
  rlang::exec(.add_bin_to_encoding, !!!args_out)
} #' vl_stack_x
#' 
#' Add stack transform to x encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param stack one of 'zero', 'center', 'normalize', NA
#' @return A modified spec
#' @export
vl_stack_x <- function(spec, stack = c('zero', 'center', 'normalize', NA)) {
  args_out <- list(spec = spec, .enc = 'x', stack = match.arg(stack))
  rlang::exec(.add_stack_to_encoding, !!!args_out)
} #' vl_stack_y
#' 
#' Add stack transform to y encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param stack one of 'zero', 'center', 'normalize', NA
#' @return A modified spec
#' @export
vl_stack_y <- function(spec, stack = c('zero', 'center', 'normalize', NA)) {
  args_out <- list(spec = spec, .enc = 'y', stack = match.arg(stack))
  rlang::exec(.add_stack_to_encoding, !!!args_out)
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
} #' vl_transform_filter
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
vl_filter <- function(spec, filter = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'filter'))
  rlang::exec(.add_transform, !!!args_out)
} #' vl_transform_calculate
#' 
#' Add CalculateTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param as The field for storing the computed formula value. (type = string)
#' @param calculate A [expression](https://vega.github.io/vega-lite/docs/types.html#expression) string. Use the variable `datum` to refer to the current data object. (type = string)
#' @return A modified spec
#' @export
vl_calculate <- function(spec, as = NULL, calculate = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'calculate'))
  rlang::exec(.add_transform, !!!args_out)
} #' vl_transform_lookup
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
vl_lookup <- function(spec, as = NULL, default = NULL, from = NULL, lookup = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'lookup'))
  rlang::exec(.add_transform, !!!args_out)
} #' vl_transform_bin
#' 
#' Add BinTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param as The output fields at which to write the start and end bin values. (type = Varies)
#' @param bin An object indicating bin properties, or simply `true` for using default bin parameters. (type = Varies)
#' @param field The data field to bin. (type = string)
#' @return A modified spec
#' @export
vl_bin <- function(spec, as = NULL, bin = NULL, field = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'bin'))
  rlang::exec(.add_transform, !!!args_out)
} #' vl_transform_timeunit
#' 
#' Add TimeUnitTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param as The output field to write the timeUnit value. (type = string)
#' @param field The data field to apply time unit. (type = string)
#' @param timeUnit The timeUnit. (type = TimeUnit)
#' @return A modified spec
#' @export
vl_timeunit <- function(spec, as = NULL, field = NULL, timeUnit = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'timeunit'))
  rlang::exec(.add_transform, !!!args_out)
} #' vl_transform_impute
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
vl_impute <- function(spec, frame = NULL, groupby = NULL, impute = NULL, key = NULL, keyvals = NULL, method = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'impute'))
  rlang::exec(.add_transform, !!!args_out)
} #' vl_transform_aggregate
#' 
#' Add AggregateTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param aggregate Array of objects that define fields to aggregate. (type = array)
#' @param groupby The data fields to group by. If not specified, a single group containing all data objects will be used. (type = array)
#' @return A modified spec
#' @export
vl_aggregate <- function(spec, aggregate = NULL, groupby = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'aggregate'))
  rlang::exec(.add_transform, !!!args_out)
} #' vl_transform_window
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
vl_window <- function(spec, frame = NULL, groupby = NULL, ignorePeers = NULL, sort = NULL, window = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'window'))
  rlang::exec(.add_transform, !!!args_out)
} #' vl_transform_joinaggregate
#' 
#' Add JoinAggregateTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param groupby The data fields for partitioning the data objects into separate groups. If unspecified, all data points will be in a single group. (type = array)
#' @param joinaggregate The definition of the fields in the join aggregate, and what calculations to use. (type = array)
#' @return A modified spec
#' @export
vl_joinaggregate <- function(spec, groupby = NULL, joinaggregate = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'joinaggregate'))
  rlang::exec(.add_transform, !!!args_out)
} #' vl_transform_stack
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
vl_stack <- function(spec, as = NULL, groupby = NULL, offset = NULL, sort = NULL, stack = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'stack'))
  rlang::exec(.add_transform, !!!args_out)
} #' vl_transform_flatten
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
vl_flatten <- function(spec, as = NULL, flatten = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'flatten'))
  rlang::exec(.add_transform, !!!args_out)
} #' vl_transform_fold
#' 
#' Add FoldTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param as The output field names for the key and value properties produced by the fold transform.
#' __Default value:__ `["key", "value"]` (type = array)
#' @param fold An array of data fields indicating the properties to fold. (type = array)
#' @return A modified spec
#' @export
vl_fold <- function(spec, as = NULL, fold = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'fold'))
  rlang::exec(.add_transform, !!!args_out)
} #' vl_transform_sample
#' 
#' Add SampleTransform to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param sample The maximum number of data objects to include in the sample.
#' 
#' __Default value:__ `1000` (type = number)
#' @return A modified spec
#' @export
vl_sample <- function(spec, sample = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = 'sample'))
  rlang::exec(.add_transform, !!!args_out)
} #' vl_FilterTransform
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
vl_FilterTransform <- function(filter = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_CalculateTransform
#' 
#' Create spec for CalculateTransform.
#' @param as The field for storing the computed formula value. (type = string)
#' @param calculate A [expression](https://vega.github.io/vega-lite/docs/types.html#expression) string. Use the variable `datum` to refer to the current data object. (type = string)
#' @return A modified spec
#' @export
vl_CalculateTransform <- function(as = NULL, calculate = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_LookupTransform
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
vl_LookupTransform <- function(as = NULL, default = NULL, from = NULL, lookup = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_BinTransform
#' 
#' Create spec for BinTransform.
#' @param as The output fields at which to write the start and end bin values. (type = Varies)
#' @param bin An object indicating bin properties, or simply `true` for using default bin parameters. (type = Varies)
#' @param field The data field to bin. (type = string)
#' @return A modified spec
#' @export
vl_BinTransform <- function(as = NULL, bin = NULL, field = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_TimeUnitTransform
#' 
#' Create spec for TimeUnitTransform.
#' @param as The output field to write the timeUnit value. (type = string)
#' @param field The data field to apply time unit. (type = string)
#' @param timeUnit The timeUnit. (type = TimeUnit)
#' @return A modified spec
#' @export
vl_TimeUnitTransform <- function(as = NULL, field = NULL, timeUnit = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_ImputeTransform
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
vl_ImputeTransform <- function(frame = NULL, groupby = NULL, impute = NULL, key = NULL, keyvals = NULL, method = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_AggregateTransform
#' 
#' Create spec for AggregateTransform.
#' @param aggregate Array of objects that define fields to aggregate. (type = array)
#' @param groupby The data fields to group by. If not specified, a single group containing all data objects will be used. (type = array)
#' @return A modified spec
#' @export
vl_AggregateTransform <- function(aggregate = NULL, groupby = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_WindowTransform
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
vl_WindowTransform <- function(frame = NULL, groupby = NULL, ignorePeers = NULL, sort = NULL, window = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_JoinAggregateTransform
#' 
#' Create spec for JoinAggregateTransform.
#' @param groupby The data fields for partitioning the data objects into separate groups. If unspecified, all data points will be in a single group. (type = array)
#' @param joinaggregate The definition of the fields in the join aggregate, and what calculations to use. (type = array)
#' @return A modified spec
#' @export
vl_JoinAggregateTransform <- function(groupby = NULL, joinaggregate = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_StackTransform
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
vl_StackTransform <- function(as = NULL, groupby = NULL, offset = NULL, sort = NULL, stack = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_FlattenTransform
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
vl_FlattenTransform <- function(as = NULL, flatten = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_FoldTransform
#' 
#' Create spec for FoldTransform.
#' @param as The output field names for the key and value properties produced by the fold transform.
#' __Default value:__ `["key", "value"]` (type = array)
#' @param fold An array of data fields indicating the properties to fold. (type = array)
#' @return A modified spec
#' @export
vl_FoldTransform <- function(as = NULL, fold = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_SampleTransform
#' 
#' Create spec for SampleTransform.
#' @param sample The maximum number of data objects to include in the sample.
#' 
#' __Default value:__ `1000` (type = number)
#' @return A modified spec
#' @export
vl_SampleTransform <- function(sample = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
} #' vl_sort_color
#' 
#' Sort color encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param value either 'ascending' or 'descending' to specify sort order (using 
#' this encoding), a list with a custom ordering, or NA to specify no sorting
#' @return A modified spec
#' @seealso [vl_sort_color_by_field()] to sort by another field, 
#' [vl_sort_color_by_encoding()] to sort by another encoding
#' @export
vl_sort_color <- function(spec, value) {
  args_out <- list(spec = spec, .enc = 'color', value = value)
  rlang::exec(.add_sort_to_encoding, !!!args_out)
} #' vl_sort_fill
#' 
#' Sort fill encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param value either 'ascending' or 'descending' to specify sort order (using 
#' this encoding), a list with a custom ordering, or NA to specify no sorting
#' @return A modified spec
#' @seealso [vl_sort_fill_by_field()] to sort by another field, 
#' [vl_sort_fill_by_encoding()] to sort by another encoding
#' @export
vl_sort_fill <- function(spec, value) {
  args_out <- list(spec = spec, .enc = 'fill', value = value)
  rlang::exec(.add_sort_to_encoding, !!!args_out)
} #' vl_sort_fillOpacity
#' 
#' Sort fillOpacity encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param value either 'ascending' or 'descending' to specify sort order (using 
#' this encoding), a list with a custom ordering, or NA to specify no sorting
#' @return A modified spec
#' @seealso [vl_sort_fillOpacity_by_field()] to sort by another field, 
#' [vl_sort_fillOpacity_by_encoding()] to sort by another encoding
#' @export
vl_sort_fillOpacity <- function(spec, value) {
  args_out <- list(spec = spec, .enc = 'fillOpacity', value = value)
  rlang::exec(.add_sort_to_encoding, !!!args_out)
} #' vl_sort_href
#' 
#' Sort href encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param value either 'ascending' or 'descending' to specify sort order (using 
#' this encoding), a list with a custom ordering, or NA to specify no sorting
#' @return A modified spec
#' @seealso [vl_sort_href_by_field()] to sort by another field, 
#' [vl_sort_href_by_encoding()] to sort by another encoding
#' @export
vl_sort_href <- function(spec, value) {
  args_out <- list(spec = spec, .enc = 'href', value = value)
  rlang::exec(.add_sort_to_encoding, !!!args_out)
} #' vl_sort_opacity
#' 
#' Sort opacity encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param value either 'ascending' or 'descending' to specify sort order (using 
#' this encoding), a list with a custom ordering, or NA to specify no sorting
#' @return A modified spec
#' @seealso [vl_sort_opacity_by_field()] to sort by another field, 
#' [vl_sort_opacity_by_encoding()] to sort by another encoding
#' @export
vl_sort_opacity <- function(spec, value) {
  args_out <- list(spec = spec, .enc = 'opacity', value = value)
  rlang::exec(.add_sort_to_encoding, !!!args_out)
} #' vl_sort_order
#' 
#' Sort order encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param value either 'ascending' or 'descending' to specify sort order (using 
#' this encoding), a list with a custom ordering, or NA to specify no sorting
#' @return A modified spec
#' @seealso [vl_sort_order_by_field()] to sort by another field, 
#' [vl_sort_order_by_encoding()] to sort by another encoding
#' @export
vl_sort_order <- function(spec, value) {
  args_out <- list(spec = spec, .enc = 'order', value = value)
  rlang::exec(.add_sort_to_encoding, !!!args_out)
} #' vl_sort_shape
#' 
#' Sort shape encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param value either 'ascending' or 'descending' to specify sort order (using 
#' this encoding), a list with a custom ordering, or NA to specify no sorting
#' @return A modified spec
#' @seealso [vl_sort_shape_by_field()] to sort by another field, 
#' [vl_sort_shape_by_encoding()] to sort by another encoding
#' @export
vl_sort_shape <- function(spec, value) {
  args_out <- list(spec = spec, .enc = 'shape', value = value)
  rlang::exec(.add_sort_to_encoding, !!!args_out)
} #' vl_sort_size
#' 
#' Sort size encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param value either 'ascending' or 'descending' to specify sort order (using 
#' this encoding), a list with a custom ordering, or NA to specify no sorting
#' @return A modified spec
#' @seealso [vl_sort_size_by_field()] to sort by another field, 
#' [vl_sort_size_by_encoding()] to sort by another encoding
#' @export
vl_sort_size <- function(spec, value) {
  args_out <- list(spec = spec, .enc = 'size', value = value)
  rlang::exec(.add_sort_to_encoding, !!!args_out)
} #' vl_sort_stroke
#' 
#' Sort stroke encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param value either 'ascending' or 'descending' to specify sort order (using 
#' this encoding), a list with a custom ordering, or NA to specify no sorting
#' @return A modified spec
#' @seealso [vl_sort_stroke_by_field()] to sort by another field, 
#' [vl_sort_stroke_by_encoding()] to sort by another encoding
#' @export
vl_sort_stroke <- function(spec, value) {
  args_out <- list(spec = spec, .enc = 'stroke', value = value)
  rlang::exec(.add_sort_to_encoding, !!!args_out)
} #' vl_sort_strokeOpacity
#' 
#' Sort strokeOpacity encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param value either 'ascending' or 'descending' to specify sort order (using 
#' this encoding), a list with a custom ordering, or NA to specify no sorting
#' @return A modified spec
#' @seealso [vl_sort_strokeOpacity_by_field()] to sort by another field, 
#' [vl_sort_strokeOpacity_by_encoding()] to sort by another encoding
#' @export
vl_sort_strokeOpacity <- function(spec, value) {
  args_out <- list(spec = spec, .enc = 'strokeOpacity', value = value)
  rlang::exec(.add_sort_to_encoding, !!!args_out)
} #' vl_sort_strokeWidth
#' 
#' Sort strokeWidth encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param value either 'ascending' or 'descending' to specify sort order (using 
#' this encoding), a list with a custom ordering, or NA to specify no sorting
#' @return A modified spec
#' @seealso [vl_sort_strokeWidth_by_field()] to sort by another field, 
#' [vl_sort_strokeWidth_by_encoding()] to sort by another encoding
#' @export
vl_sort_strokeWidth <- function(spec, value) {
  args_out <- list(spec = spec, .enc = 'strokeWidth', value = value)
  rlang::exec(.add_sort_to_encoding, !!!args_out)
} #' vl_sort_x
#' 
#' Sort x encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param value either 'ascending' or 'descending' to specify sort order (using 
#' this encoding), a list with a custom ordering, or NA to specify no sorting
#' @return A modified spec
#' @seealso [vl_sort_x_by_field()] to sort by another field, 
#' [vl_sort_x_by_encoding()] to sort by another encoding
#' @export
vl_sort_x <- function(spec, value) {
  args_out <- list(spec = spec, .enc = 'x', value = value)
  rlang::exec(.add_sort_to_encoding, !!!args_out)
} #' vl_sort_y
#' 
#' Sort y encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param value either 'ascending' or 'descending' to specify sort order (using 
#' this encoding), a list with a custom ordering, or NA to specify no sorting
#' @return A modified spec
#' @seealso [vl_sort_y_by_field()] to sort by another field, 
#' [vl_sort_y_by_encoding()] to sort by another encoding
#' @export
vl_sort_y <- function(spec, value) {
  args_out <- list(spec = spec, .enc = 'y', value = value)
  rlang::exec(.add_sort_to_encoding, !!!args_out)
} #' vl_sort_color_by_field
#' 
#' Sort color encoding by another field in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param field The data [field](https://vega.github.io/vega-lite/docs/field.html) to sort by.
#' 
#' __Default value:__ If unspecified, defaults to the field specified in the outer data reference. (type = Field)
#' @param op An [aggregate operation](https://vega.github.io/vega-lite/docs/aggregate.html#ops) to perform on the field prior to sorting (e.g., `"count"`, `"mean"` and `"median"`).
#' An aggregation is required when there are multiple values of the sort field for each encoded data field.
#' The input data objects will be aggregated, grouped by the encoded data field.
#' 
#' For a full list of operations, please see the documentation for [aggregate](https://vega.github.io/vega-lite/docs/aggregate.html#ops).
#' 
#' __Default value:__ `"sum"` for stacked plots. Otherwise, `"mean"`. (type = AggregateOp)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_color] to sort by this encoding or a custom order, 
#' [vl_sort_color_by_encoding()] to sort by another encoding
#' @export
vl_sort_color_by_field <- function(spec, field = NULL, op = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'color'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_fill_by_field
#' 
#' Sort fill encoding by another field in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param field The data [field](https://vega.github.io/vega-lite/docs/field.html) to sort by.
#' 
#' __Default value:__ If unspecified, defaults to the field specified in the outer data reference. (type = Field)
#' @param op An [aggregate operation](https://vega.github.io/vega-lite/docs/aggregate.html#ops) to perform on the field prior to sorting (e.g., `"count"`, `"mean"` and `"median"`).
#' An aggregation is required when there are multiple values of the sort field for each encoded data field.
#' The input data objects will be aggregated, grouped by the encoded data field.
#' 
#' For a full list of operations, please see the documentation for [aggregate](https://vega.github.io/vega-lite/docs/aggregate.html#ops).
#' 
#' __Default value:__ `"sum"` for stacked plots. Otherwise, `"mean"`. (type = AggregateOp)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_fill] to sort by this encoding or a custom order, 
#' [vl_sort_fill_by_encoding()] to sort by another encoding
#' @export
vl_sort_fill_by_field <- function(spec, field = NULL, op = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'fill'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_fillOpacity_by_field
#' 
#' Sort fillOpacity encoding by another field in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param field The data [field](https://vega.github.io/vega-lite/docs/field.html) to sort by.
#' 
#' __Default value:__ If unspecified, defaults to the field specified in the outer data reference. (type = Field)
#' @param op An [aggregate operation](https://vega.github.io/vega-lite/docs/aggregate.html#ops) to perform on the field prior to sorting (e.g., `"count"`, `"mean"` and `"median"`).
#' An aggregation is required when there are multiple values of the sort field for each encoded data field.
#' The input data objects will be aggregated, grouped by the encoded data field.
#' 
#' For a full list of operations, please see the documentation for [aggregate](https://vega.github.io/vega-lite/docs/aggregate.html#ops).
#' 
#' __Default value:__ `"sum"` for stacked plots. Otherwise, `"mean"`. (type = AggregateOp)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_fillOpacity] to sort by this encoding or a custom order, 
#' [vl_sort_fillOpacity_by_encoding()] to sort by another encoding
#' @export
vl_sort_fillOpacity_by_field <- function(spec, field = NULL, op = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'fillOpacity'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_href_by_field
#' 
#' Sort href encoding by another field in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param field The data [field](https://vega.github.io/vega-lite/docs/field.html) to sort by.
#' 
#' __Default value:__ If unspecified, defaults to the field specified in the outer data reference. (type = Field)
#' @param op An [aggregate operation](https://vega.github.io/vega-lite/docs/aggregate.html#ops) to perform on the field prior to sorting (e.g., `"count"`, `"mean"` and `"median"`).
#' An aggregation is required when there are multiple values of the sort field for each encoded data field.
#' The input data objects will be aggregated, grouped by the encoded data field.
#' 
#' For a full list of operations, please see the documentation for [aggregate](https://vega.github.io/vega-lite/docs/aggregate.html#ops).
#' 
#' __Default value:__ `"sum"` for stacked plots. Otherwise, `"mean"`. (type = AggregateOp)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_href] to sort by this encoding or a custom order, 
#' [vl_sort_href_by_encoding()] to sort by another encoding
#' @export
vl_sort_href_by_field <- function(spec, field = NULL, op = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'href'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_opacity_by_field
#' 
#' Sort opacity encoding by another field in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param field The data [field](https://vega.github.io/vega-lite/docs/field.html) to sort by.
#' 
#' __Default value:__ If unspecified, defaults to the field specified in the outer data reference. (type = Field)
#' @param op An [aggregate operation](https://vega.github.io/vega-lite/docs/aggregate.html#ops) to perform on the field prior to sorting (e.g., `"count"`, `"mean"` and `"median"`).
#' An aggregation is required when there are multiple values of the sort field for each encoded data field.
#' The input data objects will be aggregated, grouped by the encoded data field.
#' 
#' For a full list of operations, please see the documentation for [aggregate](https://vega.github.io/vega-lite/docs/aggregate.html#ops).
#' 
#' __Default value:__ `"sum"` for stacked plots. Otherwise, `"mean"`. (type = AggregateOp)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_opacity] to sort by this encoding or a custom order, 
#' [vl_sort_opacity_by_encoding()] to sort by another encoding
#' @export
vl_sort_opacity_by_field <- function(spec, field = NULL, op = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'opacity'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_order_by_field
#' 
#' Sort order encoding by another field in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param field The data [field](https://vega.github.io/vega-lite/docs/field.html) to sort by.
#' 
#' __Default value:__ If unspecified, defaults to the field specified in the outer data reference. (type = Field)
#' @param op An [aggregate operation](https://vega.github.io/vega-lite/docs/aggregate.html#ops) to perform on the field prior to sorting (e.g., `"count"`, `"mean"` and `"median"`).
#' An aggregation is required when there are multiple values of the sort field for each encoded data field.
#' The input data objects will be aggregated, grouped by the encoded data field.
#' 
#' For a full list of operations, please see the documentation for [aggregate](https://vega.github.io/vega-lite/docs/aggregate.html#ops).
#' 
#' __Default value:__ `"sum"` for stacked plots. Otherwise, `"mean"`. (type = AggregateOp)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_order] to sort by this encoding or a custom order, 
#' [vl_sort_order_by_encoding()] to sort by another encoding
#' @export
vl_sort_order_by_field <- function(spec, field = NULL, op = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'order'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_shape_by_field
#' 
#' Sort shape encoding by another field in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param field The data [field](https://vega.github.io/vega-lite/docs/field.html) to sort by.
#' 
#' __Default value:__ If unspecified, defaults to the field specified in the outer data reference. (type = Field)
#' @param op An [aggregate operation](https://vega.github.io/vega-lite/docs/aggregate.html#ops) to perform on the field prior to sorting (e.g., `"count"`, `"mean"` and `"median"`).
#' An aggregation is required when there are multiple values of the sort field for each encoded data field.
#' The input data objects will be aggregated, grouped by the encoded data field.
#' 
#' For a full list of operations, please see the documentation for [aggregate](https://vega.github.io/vega-lite/docs/aggregate.html#ops).
#' 
#' __Default value:__ `"sum"` for stacked plots. Otherwise, `"mean"`. (type = AggregateOp)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_shape] to sort by this encoding or a custom order, 
#' [vl_sort_shape_by_encoding()] to sort by another encoding
#' @export
vl_sort_shape_by_field <- function(spec, field = NULL, op = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'shape'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_size_by_field
#' 
#' Sort size encoding by another field in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param field The data [field](https://vega.github.io/vega-lite/docs/field.html) to sort by.
#' 
#' __Default value:__ If unspecified, defaults to the field specified in the outer data reference. (type = Field)
#' @param op An [aggregate operation](https://vega.github.io/vega-lite/docs/aggregate.html#ops) to perform on the field prior to sorting (e.g., `"count"`, `"mean"` and `"median"`).
#' An aggregation is required when there are multiple values of the sort field for each encoded data field.
#' The input data objects will be aggregated, grouped by the encoded data field.
#' 
#' For a full list of operations, please see the documentation for [aggregate](https://vega.github.io/vega-lite/docs/aggregate.html#ops).
#' 
#' __Default value:__ `"sum"` for stacked plots. Otherwise, `"mean"`. (type = AggregateOp)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_size] to sort by this encoding or a custom order, 
#' [vl_sort_size_by_encoding()] to sort by another encoding
#' @export
vl_sort_size_by_field <- function(spec, field = NULL, op = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'size'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_stroke_by_field
#' 
#' Sort stroke encoding by another field in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param field The data [field](https://vega.github.io/vega-lite/docs/field.html) to sort by.
#' 
#' __Default value:__ If unspecified, defaults to the field specified in the outer data reference. (type = Field)
#' @param op An [aggregate operation](https://vega.github.io/vega-lite/docs/aggregate.html#ops) to perform on the field prior to sorting (e.g., `"count"`, `"mean"` and `"median"`).
#' An aggregation is required when there are multiple values of the sort field for each encoded data field.
#' The input data objects will be aggregated, grouped by the encoded data field.
#' 
#' For a full list of operations, please see the documentation for [aggregate](https://vega.github.io/vega-lite/docs/aggregate.html#ops).
#' 
#' __Default value:__ `"sum"` for stacked plots. Otherwise, `"mean"`. (type = AggregateOp)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_stroke] to sort by this encoding or a custom order, 
#' [vl_sort_stroke_by_encoding()] to sort by another encoding
#' @export
vl_sort_stroke_by_field <- function(spec, field = NULL, op = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'stroke'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_strokeOpacity_by_field
#' 
#' Sort strokeOpacity encoding by another field in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param field The data [field](https://vega.github.io/vega-lite/docs/field.html) to sort by.
#' 
#' __Default value:__ If unspecified, defaults to the field specified in the outer data reference. (type = Field)
#' @param op An [aggregate operation](https://vega.github.io/vega-lite/docs/aggregate.html#ops) to perform on the field prior to sorting (e.g., `"count"`, `"mean"` and `"median"`).
#' An aggregation is required when there are multiple values of the sort field for each encoded data field.
#' The input data objects will be aggregated, grouped by the encoded data field.
#' 
#' For a full list of operations, please see the documentation for [aggregate](https://vega.github.io/vega-lite/docs/aggregate.html#ops).
#' 
#' __Default value:__ `"sum"` for stacked plots. Otherwise, `"mean"`. (type = AggregateOp)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_strokeOpacity] to sort by this encoding or a custom order, 
#' [vl_sort_strokeOpacity_by_encoding()] to sort by another encoding
#' @export
vl_sort_strokeOpacity_by_field <- function(spec, field = NULL, op = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'strokeOpacity'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_strokeWidth_by_field
#' 
#' Sort strokeWidth encoding by another field in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param field The data [field](https://vega.github.io/vega-lite/docs/field.html) to sort by.
#' 
#' __Default value:__ If unspecified, defaults to the field specified in the outer data reference. (type = Field)
#' @param op An [aggregate operation](https://vega.github.io/vega-lite/docs/aggregate.html#ops) to perform on the field prior to sorting (e.g., `"count"`, `"mean"` and `"median"`).
#' An aggregation is required when there are multiple values of the sort field for each encoded data field.
#' The input data objects will be aggregated, grouped by the encoded data field.
#' 
#' For a full list of operations, please see the documentation for [aggregate](https://vega.github.io/vega-lite/docs/aggregate.html#ops).
#' 
#' __Default value:__ `"sum"` for stacked plots. Otherwise, `"mean"`. (type = AggregateOp)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_strokeWidth] to sort by this encoding or a custom order, 
#' [vl_sort_strokeWidth_by_encoding()] to sort by another encoding
#' @export
vl_sort_strokeWidth_by_field <- function(spec, field = NULL, op = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'strokeWidth'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_x_by_field
#' 
#' Sort x encoding by another field in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param field The data [field](https://vega.github.io/vega-lite/docs/field.html) to sort by.
#' 
#' __Default value:__ If unspecified, defaults to the field specified in the outer data reference. (type = Field)
#' @param op An [aggregate operation](https://vega.github.io/vega-lite/docs/aggregate.html#ops) to perform on the field prior to sorting (e.g., `"count"`, `"mean"` and `"median"`).
#' An aggregation is required when there are multiple values of the sort field for each encoded data field.
#' The input data objects will be aggregated, grouped by the encoded data field.
#' 
#' For a full list of operations, please see the documentation for [aggregate](https://vega.github.io/vega-lite/docs/aggregate.html#ops).
#' 
#' __Default value:__ `"sum"` for stacked plots. Otherwise, `"mean"`. (type = AggregateOp)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_x] to sort by this encoding or a custom order, 
#' [vl_sort_x_by_encoding()] to sort by another encoding
#' @export
vl_sort_x_by_field <- function(spec, field = NULL, op = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'x'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_y_by_field
#' 
#' Sort y encoding by another field in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param field The data [field](https://vega.github.io/vega-lite/docs/field.html) to sort by.
#' 
#' __Default value:__ If unspecified, defaults to the field specified in the outer data reference. (type = Field)
#' @param op An [aggregate operation](https://vega.github.io/vega-lite/docs/aggregate.html#ops) to perform on the field prior to sorting (e.g., `"count"`, `"mean"` and `"median"`).
#' An aggregation is required when there are multiple values of the sort field for each encoded data field.
#' The input data objects will be aggregated, grouped by the encoded data field.
#' 
#' For a full list of operations, please see the documentation for [aggregate](https://vega.github.io/vega-lite/docs/aggregate.html#ops).
#' 
#' __Default value:__ `"sum"` for stacked plots. Otherwise, `"mean"`. (type = AggregateOp)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_y] to sort by this encoding or a custom order, 
#' [vl_sort_y_by_encoding()] to sort by another encoding
#' @export
vl_sort_y_by_field <- function(spec, field = NULL, op = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'y'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_color_by_encoding
#' 
#' Sort color encoding by another encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param encoding The [encoding channel](https://vega.github.io/vega-lite/docs/encoding.html#channels) to sort by (e.g., `"x"`, `"y"`) (type = SingleDefUnitChannel)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_color_by_field()] to sort by another field, 
#' [vl_sort_color] to sort by this encoding or a custom sort
#' @export
vl_sort_color_by_encoding <- function(spec, encoding = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'color'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_fill_by_encoding
#' 
#' Sort fill encoding by another encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param encoding The [encoding channel](https://vega.github.io/vega-lite/docs/encoding.html#channels) to sort by (e.g., `"x"`, `"y"`) (type = SingleDefUnitChannel)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_fill_by_field()] to sort by another field, 
#' [vl_sort_fill] to sort by this encoding or a custom sort
#' @export
vl_sort_fill_by_encoding <- function(spec, encoding = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'fill'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_fillOpacity_by_encoding
#' 
#' Sort fillOpacity encoding by another encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param encoding The [encoding channel](https://vega.github.io/vega-lite/docs/encoding.html#channels) to sort by (e.g., `"x"`, `"y"`) (type = SingleDefUnitChannel)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_fillOpacity_by_field()] to sort by another field, 
#' [vl_sort_fillOpacity] to sort by this encoding or a custom sort
#' @export
vl_sort_fillOpacity_by_encoding <- function(spec, encoding = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'fillOpacity'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_href_by_encoding
#' 
#' Sort href encoding by another encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param encoding The [encoding channel](https://vega.github.io/vega-lite/docs/encoding.html#channels) to sort by (e.g., `"x"`, `"y"`) (type = SingleDefUnitChannel)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_href_by_field()] to sort by another field, 
#' [vl_sort_href] to sort by this encoding or a custom sort
#' @export
vl_sort_href_by_encoding <- function(spec, encoding = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'href'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_opacity_by_encoding
#' 
#' Sort opacity encoding by another encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param encoding The [encoding channel](https://vega.github.io/vega-lite/docs/encoding.html#channels) to sort by (e.g., `"x"`, `"y"`) (type = SingleDefUnitChannel)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_opacity_by_field()] to sort by another field, 
#' [vl_sort_opacity] to sort by this encoding or a custom sort
#' @export
vl_sort_opacity_by_encoding <- function(spec, encoding = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'opacity'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_order_by_encoding
#' 
#' Sort order encoding by another encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param encoding The [encoding channel](https://vega.github.io/vega-lite/docs/encoding.html#channels) to sort by (e.g., `"x"`, `"y"`) (type = SingleDefUnitChannel)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_order_by_field()] to sort by another field, 
#' [vl_sort_order] to sort by this encoding or a custom sort
#' @export
vl_sort_order_by_encoding <- function(spec, encoding = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'order'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_shape_by_encoding
#' 
#' Sort shape encoding by another encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param encoding The [encoding channel](https://vega.github.io/vega-lite/docs/encoding.html#channels) to sort by (e.g., `"x"`, `"y"`) (type = SingleDefUnitChannel)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_shape_by_field()] to sort by another field, 
#' [vl_sort_shape] to sort by this encoding or a custom sort
#' @export
vl_sort_shape_by_encoding <- function(spec, encoding = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'shape'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_size_by_encoding
#' 
#' Sort size encoding by another encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param encoding The [encoding channel](https://vega.github.io/vega-lite/docs/encoding.html#channels) to sort by (e.g., `"x"`, `"y"`) (type = SingleDefUnitChannel)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_size_by_field()] to sort by another field, 
#' [vl_sort_size] to sort by this encoding or a custom sort
#' @export
vl_sort_size_by_encoding <- function(spec, encoding = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'size'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_stroke_by_encoding
#' 
#' Sort stroke encoding by another encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param encoding The [encoding channel](https://vega.github.io/vega-lite/docs/encoding.html#channels) to sort by (e.g., `"x"`, `"y"`) (type = SingleDefUnitChannel)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_stroke_by_field()] to sort by another field, 
#' [vl_sort_stroke] to sort by this encoding or a custom sort
#' @export
vl_sort_stroke_by_encoding <- function(spec, encoding = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'stroke'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_strokeOpacity_by_encoding
#' 
#' Sort strokeOpacity encoding by another encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param encoding The [encoding channel](https://vega.github.io/vega-lite/docs/encoding.html#channels) to sort by (e.g., `"x"`, `"y"`) (type = SingleDefUnitChannel)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_strokeOpacity_by_field()] to sort by another field, 
#' [vl_sort_strokeOpacity] to sort by this encoding or a custom sort
#' @export
vl_sort_strokeOpacity_by_encoding <- function(spec, encoding = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'strokeOpacity'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_strokeWidth_by_encoding
#' 
#' Sort strokeWidth encoding by another encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param encoding The [encoding channel](https://vega.github.io/vega-lite/docs/encoding.html#channels) to sort by (e.g., `"x"`, `"y"`) (type = SingleDefUnitChannel)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_strokeWidth_by_field()] to sort by another field, 
#' [vl_sort_strokeWidth] to sort by this encoding or a custom sort
#' @export
vl_sort_strokeWidth_by_encoding <- function(spec, encoding = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'strokeWidth'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_x_by_encoding
#' 
#' Sort x encoding by another encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param encoding The [encoding channel](https://vega.github.io/vega-lite/docs/encoding.html#channels) to sort by (e.g., `"x"`, `"y"`) (type = SingleDefUnitChannel)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_x_by_field()] to sort by another field, 
#' [vl_sort_x] to sort by this encoding or a custom sort
#' @export
vl_sort_x_by_encoding <- function(spec, encoding = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'x'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_sort_y_by_encoding
#' 
#' Sort y encoding by another encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param encoding The [encoding channel](https://vega.github.io/vega-lite/docs/encoding.html#channels) to sort by (e.g., `"x"`, `"y"`) (type = SingleDefUnitChannel)
#' @param order The sort order. One of `"ascending"` (default), `"descending"`, or `null` (no not sort). (type = Varies)
#' @return A modified spec
#' @seealso [vl_sort_y_by_field()] to sort by another field, 
#' [vl_sort_y] to sort by this encoding or a custom sort
#' @export
vl_sort_y_by_encoding <- function(spec, encoding = NULL, order = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'y'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
} #' vl_impute_x
#' 
#' Add impute transform to x encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param frame A frame specification as a two-element array used to control the window over which the specified method is applied. The array entries should either be a number indicating the offset from the current data object, or null to indicate unbounded rows preceding or following the current data object.  For example, the value `[-5, 5]` indicates that the window should include five objects preceding and five objects following the current object.
#' 
#' __Default value:__:  `[null, null]` indicating that the window includes all objects. (type = array)
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
vl_impute_x <- function(spec, frame = NULL, keyvals = NULL, method = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'x'))
  rlang::exec(.add_impute_to_encoding, !!!args_out)
} #' vl_impute_y
#' 
#' Add impute transform to y encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param frame A frame specification as a two-element array used to control the window over which the specified method is applied. The array entries should either be a number indicating the offset from the current data object, or null to indicate unbounded rows preceding or following the current data object.  For example, the value `[-5, 5]` indicates that the window should include five objects preceding and five objects following the current object.
#' 
#' __Default value:__:  `[null, null]` indicating that the window includes all objects. (type = array)
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
vl_impute_y <- function(spec, frame = NULL, keyvals = NULL, method = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'y'))
  rlang::exec(.add_impute_to_encoding, !!!args_out)
} #' vl_aggregate_color
#' 
#' Add aggregate transform to color encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_color <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'color', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_detail
#' 
#' Add aggregate transform to detail encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_detail <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'detail', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_fill
#' 
#' Add aggregate transform to fill encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_fill <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'fill', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_fillOpacity
#' 
#' Add aggregate transform to fillOpacity encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_fillOpacity <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'fillOpacity', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_href
#' 
#' Add aggregate transform to href encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_href <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'href', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_key
#' 
#' Add aggregate transform to key encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_key <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'key', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_latitude
#' 
#' Add aggregate transform to latitude encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_latitude <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'latitude', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_latitude2
#' 
#' Add aggregate transform to latitude2 encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_latitude2 <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'latitude2', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_longitude
#' 
#' Add aggregate transform to longitude encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_longitude <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'longitude', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_longitude2
#' 
#' Add aggregate transform to longitude2 encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_longitude2 <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'longitude2', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_opacity
#' 
#' Add aggregate transform to opacity encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_opacity <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'opacity', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_order
#' 
#' Add aggregate transform to order encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_order <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'order', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_shape
#' 
#' Add aggregate transform to shape encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_shape <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'shape', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_size
#' 
#' Add aggregate transform to size encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_size <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'size', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_stroke
#' 
#' Add aggregate transform to stroke encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_stroke <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'stroke', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_strokeOpacity
#' 
#' Add aggregate transform to strokeOpacity encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_strokeOpacity <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'strokeOpacity', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_strokeWidth
#' 
#' Add aggregate transform to strokeWidth encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_strokeWidth <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'strokeWidth', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_text
#' 
#' Add aggregate transform to text encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_text <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'text', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_tooltip
#' 
#' Add aggregate transform to tooltip encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_tooltip <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'tooltip', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_x
#' 
#' Add aggregate transform to x encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_x <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'x', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_x2
#' 
#' Add aggregate transform to x2 encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_x2 <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'x2', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_xError
#' 
#' Add aggregate transform to xError encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_xError <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'xError', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_xError2
#' 
#' Add aggregate transform to xError2 encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_xError2 <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'xError2', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_y
#' 
#' Add aggregate transform to y encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_y <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'y', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_y2
#' 
#' Add aggregate transform to y2 encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_y2 <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'y2', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_yError
#' 
#' Add aggregate transform to yError encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_yError <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'yError', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_aggregate_yError2
#' 
#' Add aggregate transform to yError2 encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param op Aggregation op, one of 'argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA
#' @return A modified spec
#' @export
vl_aggregate_yError2 <- function(spec, op = c('argmax', 'argmin', 'average', 'count', 'distinct', 'max', 'mean', 'median', 'min', 'missing', 'q1', 'q3', 'ci0', 'ci1', 'stderr', 'stdev', 'stdevp', 'sum', 'valid', 'values', 'variance', 'variancep', NA)) {
  args_out <- list(spec = spec, .enc = 'yError2', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
} #' vl_axis_x
#' 
#' Add axis to x encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param bandPosition An interpolation fraction indicating where, for `band` scales, axis ticks should be positioned. A value of `0` places ticks at the left edge of their bands. A value of `0.5` places ticks in the middle of their bands.
#' 
#'   __Default value:__ `0.5` (type = number)
#' @param domain A boolean flag indicating if the domain (the axis baseline) should be included as part of the axis.
#' 
#' __Default value:__ `true` (type = boolean)
#' @param domainColor Color of axis domain line.
#' 
#' __Default value:__ `"gray"`. (type = Color)
#' @param domainDash An array of alternating [stroke, space] lengths for dashed domain lines. (type = array)
#' @param domainDashOffset The pixel offset at which to start drawing with the domain dash array. (type = number)
#' @param domainOpacity Opacity of the axis domain line. (type = number)
#' @param domainWidth Stroke width of axis domain line
#' 
#' __Default value:__ `1` (type = number)
#' @param format The formatting pattern for labels. This is D3's [number format pattern](https://github.com/d3/d3-format#locale_format) for quantitative fields and D3's [time format pattern](https://github.com/d3/d3-time-format#locale_format) for time field. To override the default type, set `formatType`.
#' 
#' See the [format documentation](https://vega.github.io/vega-lite/docs/format.html) for more information.
#' 
#' __Default value:__  derived from [numberFormat](https://vega.github.io/vega-lite/docs/config.html#format) config for quantitative fields and from [timeFormat](https://vega.github.io/vega-lite/docs/config.html#format) config for temporal fields. (type = string)
#' @param formatType The format type for labels (number or time). (type = string)
#' @param grid A boolean flag indicating if grid lines should be included as part of the axis
#' 
#' __Default value:__ `true` for [continuous scales](https://vega.github.io/vega-lite/docs/scale.html#continuous) that are not binned; otherwise, `false`. (type = boolean)
#' @param gridColor Color of gridlines.
#' 
#' __Default value:__ `"lightGray"`. (type = Color)
#' @param gridDash An array of alternating [stroke, space] lengths for dashed grid lines. (type = array)
#' @param gridDashOffset The pixel offset at which to start drawing with the grid dash array. (type = number)
#' @param gridOpacity The stroke opacity of grid (value between [0,1])
#' 
#' __Default value:__ `1` (type = number)
#' @param gridWidth The grid width, in pixels.
#' 
#' __Default value:__ `1` (type = number)
#' @param labelAlign Horizontal text alignment of axis tick labels, overriding the default setting for the current axis orientation. (type = Align)
#' @param labelAngle The rotation angle of the axis labels.
#' 
#' __Default value:__ `-90` for nominal and ordinal fields; `0` otherwise. (type = number)
#' @param labelBaseline Vertical text baseline of axis tick labels, overriding the default setting for the current axis orientation. Can be `"top"`, `"middle"`, `"bottom"`, or `"alphabetic"`. (type = TextBaseline)
#' @param labelBound Indicates if labels should be hidden if they exceed the axis range. If `false` (the default) no bounds overlap analysis is performed. If `true`, labels will be hidden if they exceed the axis range by more than 1 pixel. If this property is a number, it specifies the pixel tolerance: the maximum amount by which a label bounding box may exceed the axis range.
#' 
#' __Default value:__ `false`. (type = number OR boolean)
#' @param labelColor The color of the tick label, can be in hex color code or regular color name. (type = Color)
#' @param labelFlush Indicates if the first and last axis labels should be aligned flush with the scale range. Flush alignment for a horizontal axis will left-align the first label and right-align the last label. For vertical axes, bottom and top text baselines are applied instead. If this property is a number, it also indicates the number of pixels by which to offset the first and last labels; for example, a value of 2 will flush-align the first and last labels and also push them 2 pixels outward from the center of the axis. The additional adjustment can sometimes help the labels better visually group with corresponding axis ticks.
#' 
#' __Default value:__ `true` for axis of a continuous x-scale. Otherwise, `false`. (type = boolean OR number)
#' @param labelFlushOffset Indicates the number of pixels by which to offset flush-adjusted labels. For example, a value of `2` will push flush-adjusted labels 2 pixels outward from the center of the axis. Offsets can help the labels better visually group with corresponding axis ticks.
#' 
#' __Default value:__ `0`. (type = number)
#' @param labelFont The font of the tick label. (type = string)
#' @param labelFontSize The font size of the label, in pixels. (type = number)
#' @param labelFontStyle Font style of the title. (type = FontStyle)
#' @param labelFontWeight Font weight of axis tick labels. (type = FontWeight)
#' @param labelLimit Maximum allowed pixel width of axis tick labels.
#' 
#' __Default value:__ `180` (type = number)
#' @param labelOpacity The opacity of the labels. (type = number)
#' @param labelOverlap The strategy to use for resolving overlap of axis labels. If `false` (the default), no overlap reduction is attempted. If set to `true` or `"parity"`, a strategy of removing every other label is used (this works well for standard linear axes). If set to `"greedy"`, a linear scan of the labels is performed, removing any labels that overlaps with the last visible label (this often works better for log-scaled axes).
#' 
#' __Default value:__ `true` for non-nominal fields with non-log scales; `"greedy"` for log scales; otherwise `false`. (type = LabelOverlap)
#' @param labelPadding The padding, in pixels, between axis and text labels.
#' 
#' __Default value:__ `2` (type = number)
#' @param labelSeparation The minimum separation that must be between label bounding boxes for them to be considered non-overlapping (default `0`). This property is ignored if *labelOverlap* resolution is not enabled. (type = number)
#' @param labels A boolean flag indicating if labels should be included as part of the axis.
#' 
#' __Default value:__ `true`. (type = boolean)
#' @param maxExtent The maximum extent in pixels that axis ticks and labels should use. This determines a maximum offset value for axis titles.
#' 
#' __Default value:__ `undefined`. (type = number)
#' @param minExtent The minimum extent in pixels that axis ticks and labels should use. This determines a minimum offset value for axis titles.
#' 
#' __Default value:__ `30` for y-axis; `undefined` for x-axis. (type = number)
#' @param offset The offset, in pixels, by which to displace the axis from the edge of the enclosing group or data rectangle.
#' 
#' __Default value:__ derived from the [axis config](https://vega.github.io/vega-lite/docs/config.html#facet-scale-config)'s `offset` (`0` by default) (type = number)
#' @param orient The orientation of the axis. One of `"top"`, `"bottom"`, `"left"` or `"right"`. The orientation can be used to further specialize the axis type (e.g., a y-axis oriented towards the right edge of the chart).
#' 
#' __Default value:__ `"bottom"` for x-axes and `"left"` for y-axes. (type = AxisOrient)
#' @param position The anchor position of the axis in pixels. For x-axes with top or bottom orientation, this sets the axis group x coordinate. For y-axes with left or right orientation, this sets the axis group y coordinate.
#' 
#' __Default value__: `0` (type = number)
#' @param tickColor The color of the axis's tick.
#' 
#' __Default value:__ `"gray"` (type = Color)
#' @param tickCount A desired number of ticks, for axes visualizing quantitative scales. The resulting number may be different so that values are "nice" (multiples of 2, 5, 10) and lie within the underlying scale's range. (type = number)
#' @param tickDash An array of alternating [stroke, space] lengths for dashed tick mark lines. (type = array)
#' @param tickDashOffset The pixel offset at which to start drawing with the tick mark dash array. (type = number)
#' @param tickExtra Boolean flag indicating if an extra axis tick should be added for the initial position of the axis. This flag is useful for styling axes for `band` scales such that ticks are placed on band boundaries rather in the middle of a band. Use in conjunction with `"bandPosition": 1` and an axis `"padding"` value of `0`. (type = boolean)
#' @param tickMinStep The minimum desired step between axis ticks, in terms of scale domain values. For example, a value of `1` indicates that ticks should not be less than 1 unit apart. If `tickMinStep` is specified, the `tickCount` value will be adjusted, if necessary, to enforce the minimum step value.
#' 
#' __Default value__: `undefined` (type = number)
#' @param tickOffset Position offset in pixels to apply to ticks, labels, and gridlines. (type = number)
#' @param tickOpacity Opacity of the ticks. (type = number)
#' @param tickRound Boolean flag indicating if pixel position values should be rounded to the nearest integer.
#' 
#' __Default value:__ `true` (type = boolean)
#' @param tickSize The size in pixels of axis ticks.
#' 
#' __Default value:__ `5` (type = number)
#' @param tickWidth The width, in pixels, of ticks.
#' 
#' __Default value:__ `1` (type = number)
#' @param ticks Boolean value that determines whether the axis should include ticks.
#' 
#' __Default value:__ `true` (type = boolean)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param titleAlign Horizontal text alignment of axis titles. (type = Align)
#' @param titleAnchor Text anchor position for placing axis titles. (type = TitleAnchor)
#' @param titleAngle Angle in degrees of axis titles. (type = number)
#' @param titleBaseline Vertical text baseline for axis titles. (type = TextBaseline)
#' @param titleColor Color of the title, can be in hex color code or regular color name. (type = Color)
#' @param titleFont Font of the title. (e.g., `"Helvetica Neue"`). (type = string)
#' @param titleFontSize Font size of the title. (type = number)
#' @param titleFontStyle Font style of the title. (type = FontStyle)
#' @param titleFontWeight Font weight of the title.
#' This can be either a string (e.g `"bold"`, `"normal"`) or a number (`100`, `200`, `300`, ..., `900` where `"normal"` = `400` and `"bold"` = `700`). (type = FontWeight)
#' @param titleLimit Maximum allowed pixel width of axis titles. (type = number)
#' @param titleOpacity Opacity of the axis title. (type = number)
#' @param titlePadding The padding, in pixels, between title and axis. (type = number)
#' @param titleX X-coordinate of the axis title relative to the axis group. (type = number)
#' @param titleY Y-coordinate of the axis title relative to the axis group. (type = number)
#' @param values Explicitly set the visible axis tick values. (type = Varies)
#' @param zindex A non-positive integer indicating z-index of the axis.
#' If zindex is 0, axes should be drawn behind all chart elements.
#' To put them in front, use `"zindex = 1"`.
#' 
#' __Default value:__ `1` (in front of the marks) for actual axis and `0` (behind the marks) for grids. (type = number)
#' @param remove Remove the axis?
#' @return A modified spec
#' @export
vl_axis_x <- function(spec, bandPosition = NULL, domain = NULL, domainColor = NULL, domainDash = NULL, domainDashOffset = NULL, domainOpacity = NULL, domainWidth = NULL, format = NULL, formatType = NULL, grid = NULL, gridColor = NULL, gridDash = NULL, gridDashOffset = NULL, gridOpacity = NULL, gridWidth = NULL, labelAlign = NULL, labelAngle = NULL, labelBaseline = NULL, labelBound = NULL, labelColor = NULL, labelFlush = NULL, labelFlushOffset = NULL, labelFont = NULL, labelFontSize = NULL, labelFontStyle = NULL, labelFontWeight = NULL, labelLimit = NULL, labelOpacity = NULL, labelOverlap = NULL, labelPadding = NULL, labelSeparation = NULL, labels = NULL, maxExtent = NULL, minExtent = NULL, offset = NULL, orient = NULL, position = NULL, tickColor = NULL, tickCount = NULL, tickDash = NULL, tickDashOffset = NULL, tickExtra = NULL, tickMinStep = NULL, tickOffset = NULL, tickOpacity = NULL, tickRound = NULL, tickSize = NULL, tickWidth = NULL, ticks = NULL, title = NULL, titleAlign = NULL, titleAnchor = NULL, titleAngle = NULL, titleBaseline = NULL, titleColor = NULL, titleFont = NULL, titleFontSize = NULL, titleFontStyle = NULL, titleFontWeight = NULL, titleLimit = NULL, titleOpacity = NULL, titlePadding = NULL, titleX = NULL, titleY = NULL, values = NULL, zindex = NULL, remove = FALSE) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'x'))
  rlang::exec(.add_axis_to_encoding, !!!args_out)
} #' vl_axis_y
#' 
#' Add axis to y encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param bandPosition An interpolation fraction indicating where, for `band` scales, axis ticks should be positioned. A value of `0` places ticks at the left edge of their bands. A value of `0.5` places ticks in the middle of their bands.
#' 
#'   __Default value:__ `0.5` (type = number)
#' @param domain A boolean flag indicating if the domain (the axis baseline) should be included as part of the axis.
#' 
#' __Default value:__ `true` (type = boolean)
#' @param domainColor Color of axis domain line.
#' 
#' __Default value:__ `"gray"`. (type = Color)
#' @param domainDash An array of alternating [stroke, space] lengths for dashed domain lines. (type = array)
#' @param domainDashOffset The pixel offset at which to start drawing with the domain dash array. (type = number)
#' @param domainOpacity Opacity of the axis domain line. (type = number)
#' @param domainWidth Stroke width of axis domain line
#' 
#' __Default value:__ `1` (type = number)
#' @param format The formatting pattern for labels. This is D3's [number format pattern](https://github.com/d3/d3-format#locale_format) for quantitative fields and D3's [time format pattern](https://github.com/d3/d3-time-format#locale_format) for time field. To override the default type, set `formatType`.
#' 
#' See the [format documentation](https://vega.github.io/vega-lite/docs/format.html) for more information.
#' 
#' __Default value:__  derived from [numberFormat](https://vega.github.io/vega-lite/docs/config.html#format) config for quantitative fields and from [timeFormat](https://vega.github.io/vega-lite/docs/config.html#format) config for temporal fields. (type = string)
#' @param formatType The format type for labels (number or time). (type = string)
#' @param grid A boolean flag indicating if grid lines should be included as part of the axis
#' 
#' __Default value:__ `true` for [continuous scales](https://vega.github.io/vega-lite/docs/scale.html#continuous) that are not binned; otherwise, `false`. (type = boolean)
#' @param gridColor Color of gridlines.
#' 
#' __Default value:__ `"lightGray"`. (type = Color)
#' @param gridDash An array of alternating [stroke, space] lengths for dashed grid lines. (type = array)
#' @param gridDashOffset The pixel offset at which to start drawing with the grid dash array. (type = number)
#' @param gridOpacity The stroke opacity of grid (value between [0,1])
#' 
#' __Default value:__ `1` (type = number)
#' @param gridWidth The grid width, in pixels.
#' 
#' __Default value:__ `1` (type = number)
#' @param labelAlign Horizontal text alignment of axis tick labels, overriding the default setting for the current axis orientation. (type = Align)
#' @param labelAngle The rotation angle of the axis labels.
#' 
#' __Default value:__ `-90` for nominal and ordinal fields; `0` otherwise. (type = number)
#' @param labelBaseline Vertical text baseline of axis tick labels, overriding the default setting for the current axis orientation. Can be `"top"`, `"middle"`, `"bottom"`, or `"alphabetic"`. (type = TextBaseline)
#' @param labelBound Indicates if labels should be hidden if they exceed the axis range. If `false` (the default) no bounds overlap analysis is performed. If `true`, labels will be hidden if they exceed the axis range by more than 1 pixel. If this property is a number, it specifies the pixel tolerance: the maximum amount by which a label bounding box may exceed the axis range.
#' 
#' __Default value:__ `false`. (type = number OR boolean)
#' @param labelColor The color of the tick label, can be in hex color code or regular color name. (type = Color)
#' @param labelFlush Indicates if the first and last axis labels should be aligned flush with the scale range. Flush alignment for a horizontal axis will left-align the first label and right-align the last label. For vertical axes, bottom and top text baselines are applied instead. If this property is a number, it also indicates the number of pixels by which to offset the first and last labels; for example, a value of 2 will flush-align the first and last labels and also push them 2 pixels outward from the center of the axis. The additional adjustment can sometimes help the labels better visually group with corresponding axis ticks.
#' 
#' __Default value:__ `true` for axis of a continuous x-scale. Otherwise, `false`. (type = boolean OR number)
#' @param labelFlushOffset Indicates the number of pixels by which to offset flush-adjusted labels. For example, a value of `2` will push flush-adjusted labels 2 pixels outward from the center of the axis. Offsets can help the labels better visually group with corresponding axis ticks.
#' 
#' __Default value:__ `0`. (type = number)
#' @param labelFont The font of the tick label. (type = string)
#' @param labelFontSize The font size of the label, in pixels. (type = number)
#' @param labelFontStyle Font style of the title. (type = FontStyle)
#' @param labelFontWeight Font weight of axis tick labels. (type = FontWeight)
#' @param labelLimit Maximum allowed pixel width of axis tick labels.
#' 
#' __Default value:__ `180` (type = number)
#' @param labelOpacity The opacity of the labels. (type = number)
#' @param labelOverlap The strategy to use for resolving overlap of axis labels. If `false` (the default), no overlap reduction is attempted. If set to `true` or `"parity"`, a strategy of removing every other label is used (this works well for standard linear axes). If set to `"greedy"`, a linear scan of the labels is performed, removing any labels that overlaps with the last visible label (this often works better for log-scaled axes).
#' 
#' __Default value:__ `true` for non-nominal fields with non-log scales; `"greedy"` for log scales; otherwise `false`. (type = LabelOverlap)
#' @param labelPadding The padding, in pixels, between axis and text labels.
#' 
#' __Default value:__ `2` (type = number)
#' @param labelSeparation The minimum separation that must be between label bounding boxes for them to be considered non-overlapping (default `0`). This property is ignored if *labelOverlap* resolution is not enabled. (type = number)
#' @param labels A boolean flag indicating if labels should be included as part of the axis.
#' 
#' __Default value:__ `true`. (type = boolean)
#' @param maxExtent The maximum extent in pixels that axis ticks and labels should use. This determines a maximum offset value for axis titles.
#' 
#' __Default value:__ `undefined`. (type = number)
#' @param minExtent The minimum extent in pixels that axis ticks and labels should use. This determines a minimum offset value for axis titles.
#' 
#' __Default value:__ `30` for y-axis; `undefined` for x-axis. (type = number)
#' @param offset The offset, in pixels, by which to displace the axis from the edge of the enclosing group or data rectangle.
#' 
#' __Default value:__ derived from the [axis config](https://vega.github.io/vega-lite/docs/config.html#facet-scale-config)'s `offset` (`0` by default) (type = number)
#' @param orient The orientation of the axis. One of `"top"`, `"bottom"`, `"left"` or `"right"`. The orientation can be used to further specialize the axis type (e.g., a y-axis oriented towards the right edge of the chart).
#' 
#' __Default value:__ `"bottom"` for x-axes and `"left"` for y-axes. (type = AxisOrient)
#' @param position The anchor position of the axis in pixels. For x-axes with top or bottom orientation, this sets the axis group x coordinate. For y-axes with left or right orientation, this sets the axis group y coordinate.
#' 
#' __Default value__: `0` (type = number)
#' @param tickColor The color of the axis's tick.
#' 
#' __Default value:__ `"gray"` (type = Color)
#' @param tickCount A desired number of ticks, for axes visualizing quantitative scales. The resulting number may be different so that values are "nice" (multiples of 2, 5, 10) and lie within the underlying scale's range. (type = number)
#' @param tickDash An array of alternating [stroke, space] lengths for dashed tick mark lines. (type = array)
#' @param tickDashOffset The pixel offset at which to start drawing with the tick mark dash array. (type = number)
#' @param tickExtra Boolean flag indicating if an extra axis tick should be added for the initial position of the axis. This flag is useful for styling axes for `band` scales such that ticks are placed on band boundaries rather in the middle of a band. Use in conjunction with `"bandPosition": 1` and an axis `"padding"` value of `0`. (type = boolean)
#' @param tickMinStep The minimum desired step between axis ticks, in terms of scale domain values. For example, a value of `1` indicates that ticks should not be less than 1 unit apart. If `tickMinStep` is specified, the `tickCount` value will be adjusted, if necessary, to enforce the minimum step value.
#' 
#' __Default value__: `undefined` (type = number)
#' @param tickOffset Position offset in pixels to apply to ticks, labels, and gridlines. (type = number)
#' @param tickOpacity Opacity of the ticks. (type = number)
#' @param tickRound Boolean flag indicating if pixel position values should be rounded to the nearest integer.
#' 
#' __Default value:__ `true` (type = boolean)
#' @param tickSize The size in pixels of axis ticks.
#' 
#' __Default value:__ `5` (type = number)
#' @param tickWidth The width, in pixels, of ticks.
#' 
#' __Default value:__ `1` (type = number)
#' @param ticks Boolean value that determines whether the axis should include ticks.
#' 
#' __Default value:__ `true` (type = boolean)
#' @param title A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used. (type = string OR null)
#' @param titleAlign Horizontal text alignment of axis titles. (type = Align)
#' @param titleAnchor Text anchor position for placing axis titles. (type = TitleAnchor)
#' @param titleAngle Angle in degrees of axis titles. (type = number)
#' @param titleBaseline Vertical text baseline for axis titles. (type = TextBaseline)
#' @param titleColor Color of the title, can be in hex color code or regular color name. (type = Color)
#' @param titleFont Font of the title. (e.g., `"Helvetica Neue"`). (type = string)
#' @param titleFontSize Font size of the title. (type = number)
#' @param titleFontStyle Font style of the title. (type = FontStyle)
#' @param titleFontWeight Font weight of the title.
#' This can be either a string (e.g `"bold"`, `"normal"`) or a number (`100`, `200`, `300`, ..., `900` where `"normal"` = `400` and `"bold"` = `700`). (type = FontWeight)
#' @param titleLimit Maximum allowed pixel width of axis titles. (type = number)
#' @param titleOpacity Opacity of the axis title. (type = number)
#' @param titlePadding The padding, in pixels, between title and axis. (type = number)
#' @param titleX X-coordinate of the axis title relative to the axis group. (type = number)
#' @param titleY Y-coordinate of the axis title relative to the axis group. (type = number)
#' @param values Explicitly set the visible axis tick values. (type = Varies)
#' @param zindex A non-positive integer indicating z-index of the axis.
#' If zindex is 0, axes should be drawn behind all chart elements.
#' To put them in front, use `"zindex = 1"`.
#' 
#' __Default value:__ `1` (in front of the marks) for actual axis and `0` (behind the marks) for grids. (type = number)
#' @param remove Remove the axis?
#' @return A modified spec
#' @export
vl_axis_y <- function(spec, bandPosition = NULL, domain = NULL, domainColor = NULL, domainDash = NULL, domainDashOffset = NULL, domainOpacity = NULL, domainWidth = NULL, format = NULL, formatType = NULL, grid = NULL, gridColor = NULL, gridDash = NULL, gridDashOffset = NULL, gridOpacity = NULL, gridWidth = NULL, labelAlign = NULL, labelAngle = NULL, labelBaseline = NULL, labelBound = NULL, labelColor = NULL, labelFlush = NULL, labelFlushOffset = NULL, labelFont = NULL, labelFontSize = NULL, labelFontStyle = NULL, labelFontWeight = NULL, labelLimit = NULL, labelOpacity = NULL, labelOverlap = NULL, labelPadding = NULL, labelSeparation = NULL, labels = NULL, maxExtent = NULL, minExtent = NULL, offset = NULL, orient = NULL, position = NULL, tickColor = NULL, tickCount = NULL, tickDash = NULL, tickDashOffset = NULL, tickExtra = NULL, tickMinStep = NULL, tickOffset = NULL, tickOpacity = NULL, tickRound = NULL, tickSize = NULL, tickWidth = NULL, ticks = NULL, title = NULL, titleAlign = NULL, titleAnchor = NULL, titleAngle = NULL, titleBaseline = NULL, titleColor = NULL, titleFont = NULL, titleFontSize = NULL, titleFontStyle = NULL, titleFontWeight = NULL, titleLimit = NULL, titleOpacity = NULL, titlePadding = NULL, titleX = NULL, titleY = NULL, values = NULL, zindex = NULL, remove = FALSE) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'y'))
  rlang::exec(.add_axis_to_encoding, !!!args_out)
} #' vl_scale_color
#' 
#' Add scale to color encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param base The logarithm base of the `log` scale (default `10`). (type = number)
#' @param bins An array of bin boundaries over the scale domain. If provided, axes and legends will use the bin boundaries to inform the choice of tick marks and text labels. (type = array)
#' @param clamp If `true`, values that exceed the data domain are clamped to either the minimum or maximum range value
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `clamp` (`true` by default). (type = boolean)
#' @param constant A constant determining the slope of the symlog function around zero. Only used for `symlog` scales.
#' 
#' __Default value:__ `1` (type = number)
#' @param domain Customized domain values.
#' 
#' For _quantitative_ fields, `domain` can take the form of a two-element array with minimum and maximum values.  [Piecewise scales](https://vega.github.io/vega-lite/docs/scale.html#piecewise) can be created by providing a `domain` with more than two entries.
#' If the input field is aggregated, `domain` can also be a string value `"unaggregated"`, indicating that the domain should include the raw data values prior to the aggregation.
#' 
#' For _temporal_ fields, `domain` can be a two-element array minimum and maximum values, in the form of either timestamps or the [DateTime definition objects](https://vega.github.io/vega-lite/docs/types.html#datetime).
#' 
#' For _ordinal_ and _nominal_ fields, `domain` can be an array that lists valid input values.
#' 
#' The `selection` property can be used to [interactively determine](https://vega.github.io/vega-lite/docs/selection.html#scale-domains) the scale domain. (type = Varies)
#' @param exponent The exponent of the `pow` scale. (type = number)
#' @param interpolate The interpolation method for range values. By default, a general interpolator for numbers, dates, strings and colors (in HCL space) is used. For color ranges, this property allows interpolation in alternative color spaces. Legal values include `rgb`, `hsl`, `hsl-long`, `lab`, `hcl`, `hcl-long`, `cubehelix` and `cubehelix-long` ('-long' variants use longer paths in polar coordinate spaces). If object-valued, this property accepts an object with a string-valued _type_ property and an optional numeric _gamma_ property applicable to rgb and cubehelix interpolators. For more, see the [d3-interpolate documentation](https://github.com/d3/d3-interpolate).
#' 
#' * __Default value:__ `hcl` (type = Varies)
#' @param nice Extending the domain so that it starts and ends on nice round values. This method typically modifies the scale’s domain, and may only extend the bounds to the nearest round value. Nicing is useful if the domain is computed from data and may be irregular. For example, for a domain of _[0.201479…, 0.996679…]_, a nice domain might be _[0.2, 1.0]_.
#' 
#' For quantitative scales such as linear, `nice` can be either a boolean flag or a number. If `nice` is a number, it will represent a desired tick count. This allows greater control over the step size used to extend the bounds, guaranteeing that the returned ticks will exactly cover the domain.
#' 
#' For temporal fields with time and utc scales, the `nice` value can be a string indicating the desired time interval. Legal values are `"millisecond"`, `"second"`, `"minute"`, `"hour"`, `"day"`, `"week"`, `"month"`, and `"year"`. Alternatively, `time` and `utc` scales can accept an object-valued interval specifier of the form `{"interval": "month", "step": 3}`, which includes a desired number of interval steps. Here, the domain would snap to quarter (Jan, Apr, Jul, Oct) boundaries.
#' 
#' __Default value:__ `true` for unbinned _quantitative_ fields; `false` otherwise. (type = Varies)
#' @param padding For _[continuous](https://vega.github.io/vega-lite/docs/scale.html#continuous)_ scales, expands the scale domain to accommodate the specified number of pixels on each of the scale range. The scale range must represent pixels for this parameter to function as intended. Padding adjustment is performed prior to all other adjustments, including the effects of the zero, nice, domainMin, and domainMax properties.
#' 
#' For _[band](https://vega.github.io/vega-lite/docs/scale.html#band)_ scales, shortcut for setting `paddingInner` and `paddingOuter` to the same value.
#' 
#' For _[point](https://vega.github.io/vega-lite/docs/scale.html#point)_ scales, alias for `paddingOuter`.
#' 
#' __Default value:__ For _continuous_ scales, derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `continuousPadding`.
#' For _band and point_ scales, see `paddingInner` and `paddingOuter`. (type = number)
#' @param paddingInner The inner padding (spacing) within each band step of band scales, as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' For point scale, this property is invalid as point scales do not have internal band widths (only step sizes between bands).
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingInner`. (type = number)
#' @param paddingOuter The outer padding (spacing) at the ends of the range of band and point scales,
#' as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingOuter` for band scales and `pointPadding` for point scales. (type = number)
#' @param range The range of the scale. One of:
#' 
#' - A string indicating a [pre-defined named scale range](https://vega.github.io/vega-lite/docs/scale.html#range-config) (e.g., example, `"symbol"`, or `"diverging"`).
#' 
#' - For [continuous scales](https://vega.github.io/vega-lite/docs/scale.html#continuous), two-element array indicating  minimum and maximum values, or an array with more than two entries for specifying a [piecewise scale](https://vega.github.io/vega-lite/docs/scale.html#piecewise).
#' 
#' - For [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) and [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales, an array of desired output values.
#' 
#' __Notes:__
#' 
#' 1) For color scales you can also specify a color [`scheme`](https://vega.github.io/vega-lite/docs/scale.html#scheme) instead of `range`.
#' 
#' 2) Any directly specified `range` for `x` and `y` channels will be ignored. Range can be customized via the view's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` and `height`) or via [range steps and paddings properties](#range-step) for [band](#band) and [point](#point) scales. (type = Varies)
#' @param rangeStep The distance between the starts of adjacent bands or points in [band](https://vega.github.io/vega-lite/docs/scale.html#band) and [point](https://vega.github.io/vega-lite/docs/scale.html#point) scales.
#' 
#' If `rangeStep` is `null` or if the view contains the scale's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` for `x` scales and `height` for `y` scales), `rangeStep` will be automatically determined to fit the size of the view.
#' 
#' __Default value:__  derived the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `textXRangeStep` (`90` by default) for x-scales of `text` marks and `rangeStep` (`21` by default) for x-scales of other marks and y-scales.
#' 
#' __Warning__: If `rangeStep` is `null` and the cardinality of the scale's domain is higher than `width` or `height`, the rangeStep might become less than one pixel and the mark might not appear correctly. (type = number OR null)
#' @param round If `true`, rounds numeric output values to integers. This can be helpful for snapping to the pixel grid.
#' 
#' __Default value:__ `false`. (type = boolean)
#' @param scheme A string indicating a color [scheme](https://vega.github.io/vega-lite/docs/scale.html#scheme) name (e.g., `"category10"` or `"blues"`) or a [scheme parameter object](https://vega.github.io/vega-lite/docs/scale.html#scheme-params).
#' 
#' Discrete color schemes may be used with [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) or [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales. Continuous color schemes are intended for use with color scales.
#' 
#' For the full list of supported schemes, please refer to the [Vega Scheme](https://vega.github.io/vega/docs/schemes/#reference) reference. (type = Varies)
#' @param type The type of scale.  Vega-Lite supports the following categories of scale types:
#' 
#' 1) [**Continuous Scales**](https://vega.github.io/vega-lite/docs/scale.html#continuous) -- mapping continuous domains to continuous output ranges ([`"linear"`](https://vega.github.io/vega-lite/docs/scale.html#linear), [`"pow"`](https://vega.github.io/vega-lite/docs/scale.html#pow), [`"sqrt"`](https://vega.github.io/vega-lite/docs/scale.html#sqrt), [`"symlog"`](https://vega.github.io/vega-lite/docs/scale.html#symlog), [`"log"`](https://vega.github.io/vega-lite/docs/scale.html#log), [`"time"`](https://vega.github.io/vega-lite/docs/scale.html#time), [`"utc"`](https://vega.github.io/vega-lite/docs/scale.html#utc).
#' 
#' 2) [**Discrete Scales**](https://vega.github.io/vega-lite/docs/scale.html#discrete) -- mapping discrete domains to discrete ([`"ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#ordinal)) or continuous ([`"band"`](https://vega.github.io/vega-lite/docs/scale.html#band) and [`"point"`](https://vega.github.io/vega-lite/docs/scale.html#point)) output ranges.
#' 
#' 3) [**Discretizing Scales**](https://vega.github.io/vega-lite/docs/scale.html#discretizing) -- mapping continuous domains to discrete output ranges [`"bin-ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#bin-ordinal), [`"quantile"`](https://vega.github.io/vega-lite/docs/scale.html#quantile), [`"quantize"`](https://vega.github.io/vega-lite/docs/scale.html#quantize) and [`"threshold"`](https://vega.github.io/vega-lite/docs/scale.html#threshold).
#' 
#' __Default value:__ please see the [scale type table](https://vega.github.io/vega-lite/docs/scale.html#type). (type = ScaleType)
#' @param zero If `true`, ensures that a zero baseline value is included in the scale domain.
#' 
#' __Default value:__ `true` for x and y channels if the quantitative field is not binned and no custom `domain` is provided; `false` otherwise.
#' 
#' __Note:__ Log, time, and utc scales do not support `zero`. (type = boolean)
#' @return A modified spec
#' @export
vl_scale_color <- function(spec, base = NULL, bins = NULL, clamp = NULL, constant = NULL, domain = NULL, exponent = NULL, interpolate = NULL, nice = NULL, padding = NULL, paddingInner = NULL, paddingOuter = NULL, range = NULL, rangeStep = NULL, round = NULL, scheme = NULL, type = NULL, zero = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'color'))
  rlang::exec(.add_scale_to_encoding, !!!args_out)
} #' vl_scale_fill
#' 
#' Add scale to fill encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param base The logarithm base of the `log` scale (default `10`). (type = number)
#' @param bins An array of bin boundaries over the scale domain. If provided, axes and legends will use the bin boundaries to inform the choice of tick marks and text labels. (type = array)
#' @param clamp If `true`, values that exceed the data domain are clamped to either the minimum or maximum range value
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `clamp` (`true` by default). (type = boolean)
#' @param constant A constant determining the slope of the symlog function around zero. Only used for `symlog` scales.
#' 
#' __Default value:__ `1` (type = number)
#' @param domain Customized domain values.
#' 
#' For _quantitative_ fields, `domain` can take the form of a two-element array with minimum and maximum values.  [Piecewise scales](https://vega.github.io/vega-lite/docs/scale.html#piecewise) can be created by providing a `domain` with more than two entries.
#' If the input field is aggregated, `domain` can also be a string value `"unaggregated"`, indicating that the domain should include the raw data values prior to the aggregation.
#' 
#' For _temporal_ fields, `domain` can be a two-element array minimum and maximum values, in the form of either timestamps or the [DateTime definition objects](https://vega.github.io/vega-lite/docs/types.html#datetime).
#' 
#' For _ordinal_ and _nominal_ fields, `domain` can be an array that lists valid input values.
#' 
#' The `selection` property can be used to [interactively determine](https://vega.github.io/vega-lite/docs/selection.html#scale-domains) the scale domain. (type = Varies)
#' @param exponent The exponent of the `pow` scale. (type = number)
#' @param interpolate The interpolation method for range values. By default, a general interpolator for numbers, dates, strings and colors (in HCL space) is used. For color ranges, this property allows interpolation in alternative color spaces. Legal values include `rgb`, `hsl`, `hsl-long`, `lab`, `hcl`, `hcl-long`, `cubehelix` and `cubehelix-long` ('-long' variants use longer paths in polar coordinate spaces). If object-valued, this property accepts an object with a string-valued _type_ property and an optional numeric _gamma_ property applicable to rgb and cubehelix interpolators. For more, see the [d3-interpolate documentation](https://github.com/d3/d3-interpolate).
#' 
#' * __Default value:__ `hcl` (type = Varies)
#' @param nice Extending the domain so that it starts and ends on nice round values. This method typically modifies the scale’s domain, and may only extend the bounds to the nearest round value. Nicing is useful if the domain is computed from data and may be irregular. For example, for a domain of _[0.201479…, 0.996679…]_, a nice domain might be _[0.2, 1.0]_.
#' 
#' For quantitative scales such as linear, `nice` can be either a boolean flag or a number. If `nice` is a number, it will represent a desired tick count. This allows greater control over the step size used to extend the bounds, guaranteeing that the returned ticks will exactly cover the domain.
#' 
#' For temporal fields with time and utc scales, the `nice` value can be a string indicating the desired time interval. Legal values are `"millisecond"`, `"second"`, `"minute"`, `"hour"`, `"day"`, `"week"`, `"month"`, and `"year"`. Alternatively, `time` and `utc` scales can accept an object-valued interval specifier of the form `{"interval": "month", "step": 3}`, which includes a desired number of interval steps. Here, the domain would snap to quarter (Jan, Apr, Jul, Oct) boundaries.
#' 
#' __Default value:__ `true` for unbinned _quantitative_ fields; `false` otherwise. (type = Varies)
#' @param padding For _[continuous](https://vega.github.io/vega-lite/docs/scale.html#continuous)_ scales, expands the scale domain to accommodate the specified number of pixels on each of the scale range. The scale range must represent pixels for this parameter to function as intended. Padding adjustment is performed prior to all other adjustments, including the effects of the zero, nice, domainMin, and domainMax properties.
#' 
#' For _[band](https://vega.github.io/vega-lite/docs/scale.html#band)_ scales, shortcut for setting `paddingInner` and `paddingOuter` to the same value.
#' 
#' For _[point](https://vega.github.io/vega-lite/docs/scale.html#point)_ scales, alias for `paddingOuter`.
#' 
#' __Default value:__ For _continuous_ scales, derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `continuousPadding`.
#' For _band and point_ scales, see `paddingInner` and `paddingOuter`. (type = number)
#' @param paddingInner The inner padding (spacing) within each band step of band scales, as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' For point scale, this property is invalid as point scales do not have internal band widths (only step sizes between bands).
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingInner`. (type = number)
#' @param paddingOuter The outer padding (spacing) at the ends of the range of band and point scales,
#' as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingOuter` for band scales and `pointPadding` for point scales. (type = number)
#' @param range The range of the scale. One of:
#' 
#' - A string indicating a [pre-defined named scale range](https://vega.github.io/vega-lite/docs/scale.html#range-config) (e.g., example, `"symbol"`, or `"diverging"`).
#' 
#' - For [continuous scales](https://vega.github.io/vega-lite/docs/scale.html#continuous), two-element array indicating  minimum and maximum values, or an array with more than two entries for specifying a [piecewise scale](https://vega.github.io/vega-lite/docs/scale.html#piecewise).
#' 
#' - For [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) and [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales, an array of desired output values.
#' 
#' __Notes:__
#' 
#' 1) For color scales you can also specify a color [`scheme`](https://vega.github.io/vega-lite/docs/scale.html#scheme) instead of `range`.
#' 
#' 2) Any directly specified `range` for `x` and `y` channels will be ignored. Range can be customized via the view's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` and `height`) or via [range steps and paddings properties](#range-step) for [band](#band) and [point](#point) scales. (type = Varies)
#' @param rangeStep The distance between the starts of adjacent bands or points in [band](https://vega.github.io/vega-lite/docs/scale.html#band) and [point](https://vega.github.io/vega-lite/docs/scale.html#point) scales.
#' 
#' If `rangeStep` is `null` or if the view contains the scale's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` for `x` scales and `height` for `y` scales), `rangeStep` will be automatically determined to fit the size of the view.
#' 
#' __Default value:__  derived the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `textXRangeStep` (`90` by default) for x-scales of `text` marks and `rangeStep` (`21` by default) for x-scales of other marks and y-scales.
#' 
#' __Warning__: If `rangeStep` is `null` and the cardinality of the scale's domain is higher than `width` or `height`, the rangeStep might become less than one pixel and the mark might not appear correctly. (type = number OR null)
#' @param round If `true`, rounds numeric output values to integers. This can be helpful for snapping to the pixel grid.
#' 
#' __Default value:__ `false`. (type = boolean)
#' @param scheme A string indicating a color [scheme](https://vega.github.io/vega-lite/docs/scale.html#scheme) name (e.g., `"category10"` or `"blues"`) or a [scheme parameter object](https://vega.github.io/vega-lite/docs/scale.html#scheme-params).
#' 
#' Discrete color schemes may be used with [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) or [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales. Continuous color schemes are intended for use with color scales.
#' 
#' For the full list of supported schemes, please refer to the [Vega Scheme](https://vega.github.io/vega/docs/schemes/#reference) reference. (type = Varies)
#' @param type The type of scale.  Vega-Lite supports the following categories of scale types:
#' 
#' 1) [**Continuous Scales**](https://vega.github.io/vega-lite/docs/scale.html#continuous) -- mapping continuous domains to continuous output ranges ([`"linear"`](https://vega.github.io/vega-lite/docs/scale.html#linear), [`"pow"`](https://vega.github.io/vega-lite/docs/scale.html#pow), [`"sqrt"`](https://vega.github.io/vega-lite/docs/scale.html#sqrt), [`"symlog"`](https://vega.github.io/vega-lite/docs/scale.html#symlog), [`"log"`](https://vega.github.io/vega-lite/docs/scale.html#log), [`"time"`](https://vega.github.io/vega-lite/docs/scale.html#time), [`"utc"`](https://vega.github.io/vega-lite/docs/scale.html#utc).
#' 
#' 2) [**Discrete Scales**](https://vega.github.io/vega-lite/docs/scale.html#discrete) -- mapping discrete domains to discrete ([`"ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#ordinal)) or continuous ([`"band"`](https://vega.github.io/vega-lite/docs/scale.html#band) and [`"point"`](https://vega.github.io/vega-lite/docs/scale.html#point)) output ranges.
#' 
#' 3) [**Discretizing Scales**](https://vega.github.io/vega-lite/docs/scale.html#discretizing) -- mapping continuous domains to discrete output ranges [`"bin-ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#bin-ordinal), [`"quantile"`](https://vega.github.io/vega-lite/docs/scale.html#quantile), [`"quantize"`](https://vega.github.io/vega-lite/docs/scale.html#quantize) and [`"threshold"`](https://vega.github.io/vega-lite/docs/scale.html#threshold).
#' 
#' __Default value:__ please see the [scale type table](https://vega.github.io/vega-lite/docs/scale.html#type). (type = ScaleType)
#' @param zero If `true`, ensures that a zero baseline value is included in the scale domain.
#' 
#' __Default value:__ `true` for x and y channels if the quantitative field is not binned and no custom `domain` is provided; `false` otherwise.
#' 
#' __Note:__ Log, time, and utc scales do not support `zero`. (type = boolean)
#' @return A modified spec
#' @export
vl_scale_fill <- function(spec, base = NULL, bins = NULL, clamp = NULL, constant = NULL, domain = NULL, exponent = NULL, interpolate = NULL, nice = NULL, padding = NULL, paddingInner = NULL, paddingOuter = NULL, range = NULL, rangeStep = NULL, round = NULL, scheme = NULL, type = NULL, zero = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'fill'))
  rlang::exec(.add_scale_to_encoding, !!!args_out)
} #' vl_scale_fillOpacity
#' 
#' Add scale to fillOpacity encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param base The logarithm base of the `log` scale (default `10`). (type = number)
#' @param bins An array of bin boundaries over the scale domain. If provided, axes and legends will use the bin boundaries to inform the choice of tick marks and text labels. (type = array)
#' @param clamp If `true`, values that exceed the data domain are clamped to either the minimum or maximum range value
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `clamp` (`true` by default). (type = boolean)
#' @param constant A constant determining the slope of the symlog function around zero. Only used for `symlog` scales.
#' 
#' __Default value:__ `1` (type = number)
#' @param domain Customized domain values.
#' 
#' For _quantitative_ fields, `domain` can take the form of a two-element array with minimum and maximum values.  [Piecewise scales](https://vega.github.io/vega-lite/docs/scale.html#piecewise) can be created by providing a `domain` with more than two entries.
#' If the input field is aggregated, `domain` can also be a string value `"unaggregated"`, indicating that the domain should include the raw data values prior to the aggregation.
#' 
#' For _temporal_ fields, `domain` can be a two-element array minimum and maximum values, in the form of either timestamps or the [DateTime definition objects](https://vega.github.io/vega-lite/docs/types.html#datetime).
#' 
#' For _ordinal_ and _nominal_ fields, `domain` can be an array that lists valid input values.
#' 
#' The `selection` property can be used to [interactively determine](https://vega.github.io/vega-lite/docs/selection.html#scale-domains) the scale domain. (type = Varies)
#' @param exponent The exponent of the `pow` scale. (type = number)
#' @param interpolate The interpolation method for range values. By default, a general interpolator for numbers, dates, strings and colors (in HCL space) is used. For color ranges, this property allows interpolation in alternative color spaces. Legal values include `rgb`, `hsl`, `hsl-long`, `lab`, `hcl`, `hcl-long`, `cubehelix` and `cubehelix-long` ('-long' variants use longer paths in polar coordinate spaces). If object-valued, this property accepts an object with a string-valued _type_ property and an optional numeric _gamma_ property applicable to rgb and cubehelix interpolators. For more, see the [d3-interpolate documentation](https://github.com/d3/d3-interpolate).
#' 
#' * __Default value:__ `hcl` (type = Varies)
#' @param nice Extending the domain so that it starts and ends on nice round values. This method typically modifies the scale’s domain, and may only extend the bounds to the nearest round value. Nicing is useful if the domain is computed from data and may be irregular. For example, for a domain of _[0.201479…, 0.996679…]_, a nice domain might be _[0.2, 1.0]_.
#' 
#' For quantitative scales such as linear, `nice` can be either a boolean flag or a number. If `nice` is a number, it will represent a desired tick count. This allows greater control over the step size used to extend the bounds, guaranteeing that the returned ticks will exactly cover the domain.
#' 
#' For temporal fields with time and utc scales, the `nice` value can be a string indicating the desired time interval. Legal values are `"millisecond"`, `"second"`, `"minute"`, `"hour"`, `"day"`, `"week"`, `"month"`, and `"year"`. Alternatively, `time` and `utc` scales can accept an object-valued interval specifier of the form `{"interval": "month", "step": 3}`, which includes a desired number of interval steps. Here, the domain would snap to quarter (Jan, Apr, Jul, Oct) boundaries.
#' 
#' __Default value:__ `true` for unbinned _quantitative_ fields; `false` otherwise. (type = Varies)
#' @param padding For _[continuous](https://vega.github.io/vega-lite/docs/scale.html#continuous)_ scales, expands the scale domain to accommodate the specified number of pixels on each of the scale range. The scale range must represent pixels for this parameter to function as intended. Padding adjustment is performed prior to all other adjustments, including the effects of the zero, nice, domainMin, and domainMax properties.
#' 
#' For _[band](https://vega.github.io/vega-lite/docs/scale.html#band)_ scales, shortcut for setting `paddingInner` and `paddingOuter` to the same value.
#' 
#' For _[point](https://vega.github.io/vega-lite/docs/scale.html#point)_ scales, alias for `paddingOuter`.
#' 
#' __Default value:__ For _continuous_ scales, derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `continuousPadding`.
#' For _band and point_ scales, see `paddingInner` and `paddingOuter`. (type = number)
#' @param paddingInner The inner padding (spacing) within each band step of band scales, as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' For point scale, this property is invalid as point scales do not have internal band widths (only step sizes between bands).
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingInner`. (type = number)
#' @param paddingOuter The outer padding (spacing) at the ends of the range of band and point scales,
#' as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingOuter` for band scales and `pointPadding` for point scales. (type = number)
#' @param range The range of the scale. One of:
#' 
#' - A string indicating a [pre-defined named scale range](https://vega.github.io/vega-lite/docs/scale.html#range-config) (e.g., example, `"symbol"`, or `"diverging"`).
#' 
#' - For [continuous scales](https://vega.github.io/vega-lite/docs/scale.html#continuous), two-element array indicating  minimum and maximum values, or an array with more than two entries for specifying a [piecewise scale](https://vega.github.io/vega-lite/docs/scale.html#piecewise).
#' 
#' - For [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) and [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales, an array of desired output values.
#' 
#' __Notes:__
#' 
#' 1) For color scales you can also specify a color [`scheme`](https://vega.github.io/vega-lite/docs/scale.html#scheme) instead of `range`.
#' 
#' 2) Any directly specified `range` for `x` and `y` channels will be ignored. Range can be customized via the view's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` and `height`) or via [range steps and paddings properties](#range-step) for [band](#band) and [point](#point) scales. (type = Varies)
#' @param rangeStep The distance between the starts of adjacent bands or points in [band](https://vega.github.io/vega-lite/docs/scale.html#band) and [point](https://vega.github.io/vega-lite/docs/scale.html#point) scales.
#' 
#' If `rangeStep` is `null` or if the view contains the scale's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` for `x` scales and `height` for `y` scales), `rangeStep` will be automatically determined to fit the size of the view.
#' 
#' __Default value:__  derived the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `textXRangeStep` (`90` by default) for x-scales of `text` marks and `rangeStep` (`21` by default) for x-scales of other marks and y-scales.
#' 
#' __Warning__: If `rangeStep` is `null` and the cardinality of the scale's domain is higher than `width` or `height`, the rangeStep might become less than one pixel and the mark might not appear correctly. (type = number OR null)
#' @param round If `true`, rounds numeric output values to integers. This can be helpful for snapping to the pixel grid.
#' 
#' __Default value:__ `false`. (type = boolean)
#' @param scheme A string indicating a color [scheme](https://vega.github.io/vega-lite/docs/scale.html#scheme) name (e.g., `"category10"` or `"blues"`) or a [scheme parameter object](https://vega.github.io/vega-lite/docs/scale.html#scheme-params).
#' 
#' Discrete color schemes may be used with [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) or [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales. Continuous color schemes are intended for use with color scales.
#' 
#' For the full list of supported schemes, please refer to the [Vega Scheme](https://vega.github.io/vega/docs/schemes/#reference) reference. (type = Varies)
#' @param type The type of scale.  Vega-Lite supports the following categories of scale types:
#' 
#' 1) [**Continuous Scales**](https://vega.github.io/vega-lite/docs/scale.html#continuous) -- mapping continuous domains to continuous output ranges ([`"linear"`](https://vega.github.io/vega-lite/docs/scale.html#linear), [`"pow"`](https://vega.github.io/vega-lite/docs/scale.html#pow), [`"sqrt"`](https://vega.github.io/vega-lite/docs/scale.html#sqrt), [`"symlog"`](https://vega.github.io/vega-lite/docs/scale.html#symlog), [`"log"`](https://vega.github.io/vega-lite/docs/scale.html#log), [`"time"`](https://vega.github.io/vega-lite/docs/scale.html#time), [`"utc"`](https://vega.github.io/vega-lite/docs/scale.html#utc).
#' 
#' 2) [**Discrete Scales**](https://vega.github.io/vega-lite/docs/scale.html#discrete) -- mapping discrete domains to discrete ([`"ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#ordinal)) or continuous ([`"band"`](https://vega.github.io/vega-lite/docs/scale.html#band) and [`"point"`](https://vega.github.io/vega-lite/docs/scale.html#point)) output ranges.
#' 
#' 3) [**Discretizing Scales**](https://vega.github.io/vega-lite/docs/scale.html#discretizing) -- mapping continuous domains to discrete output ranges [`"bin-ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#bin-ordinal), [`"quantile"`](https://vega.github.io/vega-lite/docs/scale.html#quantile), [`"quantize"`](https://vega.github.io/vega-lite/docs/scale.html#quantize) and [`"threshold"`](https://vega.github.io/vega-lite/docs/scale.html#threshold).
#' 
#' __Default value:__ please see the [scale type table](https://vega.github.io/vega-lite/docs/scale.html#type). (type = ScaleType)
#' @param zero If `true`, ensures that a zero baseline value is included in the scale domain.
#' 
#' __Default value:__ `true` for x and y channels if the quantitative field is not binned and no custom `domain` is provided; `false` otherwise.
#' 
#' __Note:__ Log, time, and utc scales do not support `zero`. (type = boolean)
#' @return A modified spec
#' @export
vl_scale_fillOpacity <- function(spec, base = NULL, bins = NULL, clamp = NULL, constant = NULL, domain = NULL, exponent = NULL, interpolate = NULL, nice = NULL, padding = NULL, paddingInner = NULL, paddingOuter = NULL, range = NULL, rangeStep = NULL, round = NULL, scheme = NULL, type = NULL, zero = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'fillOpacity'))
  rlang::exec(.add_scale_to_encoding, !!!args_out)
} #' vl_scale_href
#' 
#' Add scale to href encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param base The logarithm base of the `log` scale (default `10`). (type = number)
#' @param bins An array of bin boundaries over the scale domain. If provided, axes and legends will use the bin boundaries to inform the choice of tick marks and text labels. (type = array)
#' @param clamp If `true`, values that exceed the data domain are clamped to either the minimum or maximum range value
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `clamp` (`true` by default). (type = boolean)
#' @param constant A constant determining the slope of the symlog function around zero. Only used for `symlog` scales.
#' 
#' __Default value:__ `1` (type = number)
#' @param domain Customized domain values.
#' 
#' For _quantitative_ fields, `domain` can take the form of a two-element array with minimum and maximum values.  [Piecewise scales](https://vega.github.io/vega-lite/docs/scale.html#piecewise) can be created by providing a `domain` with more than two entries.
#' If the input field is aggregated, `domain` can also be a string value `"unaggregated"`, indicating that the domain should include the raw data values prior to the aggregation.
#' 
#' For _temporal_ fields, `domain` can be a two-element array minimum and maximum values, in the form of either timestamps or the [DateTime definition objects](https://vega.github.io/vega-lite/docs/types.html#datetime).
#' 
#' For _ordinal_ and _nominal_ fields, `domain` can be an array that lists valid input values.
#' 
#' The `selection` property can be used to [interactively determine](https://vega.github.io/vega-lite/docs/selection.html#scale-domains) the scale domain. (type = Varies)
#' @param exponent The exponent of the `pow` scale. (type = number)
#' @param interpolate The interpolation method for range values. By default, a general interpolator for numbers, dates, strings and colors (in HCL space) is used. For color ranges, this property allows interpolation in alternative color spaces. Legal values include `rgb`, `hsl`, `hsl-long`, `lab`, `hcl`, `hcl-long`, `cubehelix` and `cubehelix-long` ('-long' variants use longer paths in polar coordinate spaces). If object-valued, this property accepts an object with a string-valued _type_ property and an optional numeric _gamma_ property applicable to rgb and cubehelix interpolators. For more, see the [d3-interpolate documentation](https://github.com/d3/d3-interpolate).
#' 
#' * __Default value:__ `hcl` (type = Varies)
#' @param nice Extending the domain so that it starts and ends on nice round values. This method typically modifies the scale’s domain, and may only extend the bounds to the nearest round value. Nicing is useful if the domain is computed from data and may be irregular. For example, for a domain of _[0.201479…, 0.996679…]_, a nice domain might be _[0.2, 1.0]_.
#' 
#' For quantitative scales such as linear, `nice` can be either a boolean flag or a number. If `nice` is a number, it will represent a desired tick count. This allows greater control over the step size used to extend the bounds, guaranteeing that the returned ticks will exactly cover the domain.
#' 
#' For temporal fields with time and utc scales, the `nice` value can be a string indicating the desired time interval. Legal values are `"millisecond"`, `"second"`, `"minute"`, `"hour"`, `"day"`, `"week"`, `"month"`, and `"year"`. Alternatively, `time` and `utc` scales can accept an object-valued interval specifier of the form `{"interval": "month", "step": 3}`, which includes a desired number of interval steps. Here, the domain would snap to quarter (Jan, Apr, Jul, Oct) boundaries.
#' 
#' __Default value:__ `true` for unbinned _quantitative_ fields; `false` otherwise. (type = Varies)
#' @param padding For _[continuous](https://vega.github.io/vega-lite/docs/scale.html#continuous)_ scales, expands the scale domain to accommodate the specified number of pixels on each of the scale range. The scale range must represent pixels for this parameter to function as intended. Padding adjustment is performed prior to all other adjustments, including the effects of the zero, nice, domainMin, and domainMax properties.
#' 
#' For _[band](https://vega.github.io/vega-lite/docs/scale.html#band)_ scales, shortcut for setting `paddingInner` and `paddingOuter` to the same value.
#' 
#' For _[point](https://vega.github.io/vega-lite/docs/scale.html#point)_ scales, alias for `paddingOuter`.
#' 
#' __Default value:__ For _continuous_ scales, derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `continuousPadding`.
#' For _band and point_ scales, see `paddingInner` and `paddingOuter`. (type = number)
#' @param paddingInner The inner padding (spacing) within each band step of band scales, as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' For point scale, this property is invalid as point scales do not have internal band widths (only step sizes between bands).
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingInner`. (type = number)
#' @param paddingOuter The outer padding (spacing) at the ends of the range of band and point scales,
#' as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingOuter` for band scales and `pointPadding` for point scales. (type = number)
#' @param range The range of the scale. One of:
#' 
#' - A string indicating a [pre-defined named scale range](https://vega.github.io/vega-lite/docs/scale.html#range-config) (e.g., example, `"symbol"`, or `"diverging"`).
#' 
#' - For [continuous scales](https://vega.github.io/vega-lite/docs/scale.html#continuous), two-element array indicating  minimum and maximum values, or an array with more than two entries for specifying a [piecewise scale](https://vega.github.io/vega-lite/docs/scale.html#piecewise).
#' 
#' - For [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) and [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales, an array of desired output values.
#' 
#' __Notes:__
#' 
#' 1) For color scales you can also specify a color [`scheme`](https://vega.github.io/vega-lite/docs/scale.html#scheme) instead of `range`.
#' 
#' 2) Any directly specified `range` for `x` and `y` channels will be ignored. Range can be customized via the view's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` and `height`) or via [range steps and paddings properties](#range-step) for [band](#band) and [point](#point) scales. (type = Varies)
#' @param rangeStep The distance between the starts of adjacent bands or points in [band](https://vega.github.io/vega-lite/docs/scale.html#band) and [point](https://vega.github.io/vega-lite/docs/scale.html#point) scales.
#' 
#' If `rangeStep` is `null` or if the view contains the scale's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` for `x` scales and `height` for `y` scales), `rangeStep` will be automatically determined to fit the size of the view.
#' 
#' __Default value:__  derived the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `textXRangeStep` (`90` by default) for x-scales of `text` marks and `rangeStep` (`21` by default) for x-scales of other marks and y-scales.
#' 
#' __Warning__: If `rangeStep` is `null` and the cardinality of the scale's domain is higher than `width` or `height`, the rangeStep might become less than one pixel and the mark might not appear correctly. (type = number OR null)
#' @param round If `true`, rounds numeric output values to integers. This can be helpful for snapping to the pixel grid.
#' 
#' __Default value:__ `false`. (type = boolean)
#' @param scheme A string indicating a color [scheme](https://vega.github.io/vega-lite/docs/scale.html#scheme) name (e.g., `"category10"` or `"blues"`) or a [scheme parameter object](https://vega.github.io/vega-lite/docs/scale.html#scheme-params).
#' 
#' Discrete color schemes may be used with [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) or [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales. Continuous color schemes are intended for use with color scales.
#' 
#' For the full list of supported schemes, please refer to the [Vega Scheme](https://vega.github.io/vega/docs/schemes/#reference) reference. (type = Varies)
#' @param type The type of scale.  Vega-Lite supports the following categories of scale types:
#' 
#' 1) [**Continuous Scales**](https://vega.github.io/vega-lite/docs/scale.html#continuous) -- mapping continuous domains to continuous output ranges ([`"linear"`](https://vega.github.io/vega-lite/docs/scale.html#linear), [`"pow"`](https://vega.github.io/vega-lite/docs/scale.html#pow), [`"sqrt"`](https://vega.github.io/vega-lite/docs/scale.html#sqrt), [`"symlog"`](https://vega.github.io/vega-lite/docs/scale.html#symlog), [`"log"`](https://vega.github.io/vega-lite/docs/scale.html#log), [`"time"`](https://vega.github.io/vega-lite/docs/scale.html#time), [`"utc"`](https://vega.github.io/vega-lite/docs/scale.html#utc).
#' 
#' 2) [**Discrete Scales**](https://vega.github.io/vega-lite/docs/scale.html#discrete) -- mapping discrete domains to discrete ([`"ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#ordinal)) or continuous ([`"band"`](https://vega.github.io/vega-lite/docs/scale.html#band) and [`"point"`](https://vega.github.io/vega-lite/docs/scale.html#point)) output ranges.
#' 
#' 3) [**Discretizing Scales**](https://vega.github.io/vega-lite/docs/scale.html#discretizing) -- mapping continuous domains to discrete output ranges [`"bin-ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#bin-ordinal), [`"quantile"`](https://vega.github.io/vega-lite/docs/scale.html#quantile), [`"quantize"`](https://vega.github.io/vega-lite/docs/scale.html#quantize) and [`"threshold"`](https://vega.github.io/vega-lite/docs/scale.html#threshold).
#' 
#' __Default value:__ please see the [scale type table](https://vega.github.io/vega-lite/docs/scale.html#type). (type = ScaleType)
#' @param zero If `true`, ensures that a zero baseline value is included in the scale domain.
#' 
#' __Default value:__ `true` for x and y channels if the quantitative field is not binned and no custom `domain` is provided; `false` otherwise.
#' 
#' __Note:__ Log, time, and utc scales do not support `zero`. (type = boolean)
#' @return A modified spec
#' @export
vl_scale_href <- function(spec, base = NULL, bins = NULL, clamp = NULL, constant = NULL, domain = NULL, exponent = NULL, interpolate = NULL, nice = NULL, padding = NULL, paddingInner = NULL, paddingOuter = NULL, range = NULL, rangeStep = NULL, round = NULL, scheme = NULL, type = NULL, zero = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'href'))
  rlang::exec(.add_scale_to_encoding, !!!args_out)
} #' vl_scale_opacity
#' 
#' Add scale to opacity encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param base The logarithm base of the `log` scale (default `10`). (type = number)
#' @param bins An array of bin boundaries over the scale domain. If provided, axes and legends will use the bin boundaries to inform the choice of tick marks and text labels. (type = array)
#' @param clamp If `true`, values that exceed the data domain are clamped to either the minimum or maximum range value
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `clamp` (`true` by default). (type = boolean)
#' @param constant A constant determining the slope of the symlog function around zero. Only used for `symlog` scales.
#' 
#' __Default value:__ `1` (type = number)
#' @param domain Customized domain values.
#' 
#' For _quantitative_ fields, `domain` can take the form of a two-element array with minimum and maximum values.  [Piecewise scales](https://vega.github.io/vega-lite/docs/scale.html#piecewise) can be created by providing a `domain` with more than two entries.
#' If the input field is aggregated, `domain` can also be a string value `"unaggregated"`, indicating that the domain should include the raw data values prior to the aggregation.
#' 
#' For _temporal_ fields, `domain` can be a two-element array minimum and maximum values, in the form of either timestamps or the [DateTime definition objects](https://vega.github.io/vega-lite/docs/types.html#datetime).
#' 
#' For _ordinal_ and _nominal_ fields, `domain` can be an array that lists valid input values.
#' 
#' The `selection` property can be used to [interactively determine](https://vega.github.io/vega-lite/docs/selection.html#scale-domains) the scale domain. (type = Varies)
#' @param exponent The exponent of the `pow` scale. (type = number)
#' @param interpolate The interpolation method for range values. By default, a general interpolator for numbers, dates, strings and colors (in HCL space) is used. For color ranges, this property allows interpolation in alternative color spaces. Legal values include `rgb`, `hsl`, `hsl-long`, `lab`, `hcl`, `hcl-long`, `cubehelix` and `cubehelix-long` ('-long' variants use longer paths in polar coordinate spaces). If object-valued, this property accepts an object with a string-valued _type_ property and an optional numeric _gamma_ property applicable to rgb and cubehelix interpolators. For more, see the [d3-interpolate documentation](https://github.com/d3/d3-interpolate).
#' 
#' * __Default value:__ `hcl` (type = Varies)
#' @param nice Extending the domain so that it starts and ends on nice round values. This method typically modifies the scale’s domain, and may only extend the bounds to the nearest round value. Nicing is useful if the domain is computed from data and may be irregular. For example, for a domain of _[0.201479…, 0.996679…]_, a nice domain might be _[0.2, 1.0]_.
#' 
#' For quantitative scales such as linear, `nice` can be either a boolean flag or a number. If `nice` is a number, it will represent a desired tick count. This allows greater control over the step size used to extend the bounds, guaranteeing that the returned ticks will exactly cover the domain.
#' 
#' For temporal fields with time and utc scales, the `nice` value can be a string indicating the desired time interval. Legal values are `"millisecond"`, `"second"`, `"minute"`, `"hour"`, `"day"`, `"week"`, `"month"`, and `"year"`. Alternatively, `time` and `utc` scales can accept an object-valued interval specifier of the form `{"interval": "month", "step": 3}`, which includes a desired number of interval steps. Here, the domain would snap to quarter (Jan, Apr, Jul, Oct) boundaries.
#' 
#' __Default value:__ `true` for unbinned _quantitative_ fields; `false` otherwise. (type = Varies)
#' @param padding For _[continuous](https://vega.github.io/vega-lite/docs/scale.html#continuous)_ scales, expands the scale domain to accommodate the specified number of pixels on each of the scale range. The scale range must represent pixels for this parameter to function as intended. Padding adjustment is performed prior to all other adjustments, including the effects of the zero, nice, domainMin, and domainMax properties.
#' 
#' For _[band](https://vega.github.io/vega-lite/docs/scale.html#band)_ scales, shortcut for setting `paddingInner` and `paddingOuter` to the same value.
#' 
#' For _[point](https://vega.github.io/vega-lite/docs/scale.html#point)_ scales, alias for `paddingOuter`.
#' 
#' __Default value:__ For _continuous_ scales, derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `continuousPadding`.
#' For _band and point_ scales, see `paddingInner` and `paddingOuter`. (type = number)
#' @param paddingInner The inner padding (spacing) within each band step of band scales, as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' For point scale, this property is invalid as point scales do not have internal band widths (only step sizes between bands).
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingInner`. (type = number)
#' @param paddingOuter The outer padding (spacing) at the ends of the range of band and point scales,
#' as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingOuter` for band scales and `pointPadding` for point scales. (type = number)
#' @param range The range of the scale. One of:
#' 
#' - A string indicating a [pre-defined named scale range](https://vega.github.io/vega-lite/docs/scale.html#range-config) (e.g., example, `"symbol"`, or `"diverging"`).
#' 
#' - For [continuous scales](https://vega.github.io/vega-lite/docs/scale.html#continuous), two-element array indicating  minimum and maximum values, or an array with more than two entries for specifying a [piecewise scale](https://vega.github.io/vega-lite/docs/scale.html#piecewise).
#' 
#' - For [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) and [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales, an array of desired output values.
#' 
#' __Notes:__
#' 
#' 1) For color scales you can also specify a color [`scheme`](https://vega.github.io/vega-lite/docs/scale.html#scheme) instead of `range`.
#' 
#' 2) Any directly specified `range` for `x` and `y` channels will be ignored. Range can be customized via the view's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` and `height`) or via [range steps and paddings properties](#range-step) for [band](#band) and [point](#point) scales. (type = Varies)
#' @param rangeStep The distance between the starts of adjacent bands or points in [band](https://vega.github.io/vega-lite/docs/scale.html#band) and [point](https://vega.github.io/vega-lite/docs/scale.html#point) scales.
#' 
#' If `rangeStep` is `null` or if the view contains the scale's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` for `x` scales and `height` for `y` scales), `rangeStep` will be automatically determined to fit the size of the view.
#' 
#' __Default value:__  derived the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `textXRangeStep` (`90` by default) for x-scales of `text` marks and `rangeStep` (`21` by default) for x-scales of other marks and y-scales.
#' 
#' __Warning__: If `rangeStep` is `null` and the cardinality of the scale's domain is higher than `width` or `height`, the rangeStep might become less than one pixel and the mark might not appear correctly. (type = number OR null)
#' @param round If `true`, rounds numeric output values to integers. This can be helpful for snapping to the pixel grid.
#' 
#' __Default value:__ `false`. (type = boolean)
#' @param scheme A string indicating a color [scheme](https://vega.github.io/vega-lite/docs/scale.html#scheme) name (e.g., `"category10"` or `"blues"`) or a [scheme parameter object](https://vega.github.io/vega-lite/docs/scale.html#scheme-params).
#' 
#' Discrete color schemes may be used with [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) or [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales. Continuous color schemes are intended for use with color scales.
#' 
#' For the full list of supported schemes, please refer to the [Vega Scheme](https://vega.github.io/vega/docs/schemes/#reference) reference. (type = Varies)
#' @param type The type of scale.  Vega-Lite supports the following categories of scale types:
#' 
#' 1) [**Continuous Scales**](https://vega.github.io/vega-lite/docs/scale.html#continuous) -- mapping continuous domains to continuous output ranges ([`"linear"`](https://vega.github.io/vega-lite/docs/scale.html#linear), [`"pow"`](https://vega.github.io/vega-lite/docs/scale.html#pow), [`"sqrt"`](https://vega.github.io/vega-lite/docs/scale.html#sqrt), [`"symlog"`](https://vega.github.io/vega-lite/docs/scale.html#symlog), [`"log"`](https://vega.github.io/vega-lite/docs/scale.html#log), [`"time"`](https://vega.github.io/vega-lite/docs/scale.html#time), [`"utc"`](https://vega.github.io/vega-lite/docs/scale.html#utc).
#' 
#' 2) [**Discrete Scales**](https://vega.github.io/vega-lite/docs/scale.html#discrete) -- mapping discrete domains to discrete ([`"ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#ordinal)) or continuous ([`"band"`](https://vega.github.io/vega-lite/docs/scale.html#band) and [`"point"`](https://vega.github.io/vega-lite/docs/scale.html#point)) output ranges.
#' 
#' 3) [**Discretizing Scales**](https://vega.github.io/vega-lite/docs/scale.html#discretizing) -- mapping continuous domains to discrete output ranges [`"bin-ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#bin-ordinal), [`"quantile"`](https://vega.github.io/vega-lite/docs/scale.html#quantile), [`"quantize"`](https://vega.github.io/vega-lite/docs/scale.html#quantize) and [`"threshold"`](https://vega.github.io/vega-lite/docs/scale.html#threshold).
#' 
#' __Default value:__ please see the [scale type table](https://vega.github.io/vega-lite/docs/scale.html#type). (type = ScaleType)
#' @param zero If `true`, ensures that a zero baseline value is included in the scale domain.
#' 
#' __Default value:__ `true` for x and y channels if the quantitative field is not binned and no custom `domain` is provided; `false` otherwise.
#' 
#' __Note:__ Log, time, and utc scales do not support `zero`. (type = boolean)
#' @return A modified spec
#' @export
vl_scale_opacity <- function(spec, base = NULL, bins = NULL, clamp = NULL, constant = NULL, domain = NULL, exponent = NULL, interpolate = NULL, nice = NULL, padding = NULL, paddingInner = NULL, paddingOuter = NULL, range = NULL, rangeStep = NULL, round = NULL, scheme = NULL, type = NULL, zero = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'opacity'))
  rlang::exec(.add_scale_to_encoding, !!!args_out)
} #' vl_scale_shape
#' 
#' Add scale to shape encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param base The logarithm base of the `log` scale (default `10`). (type = number)
#' @param bins An array of bin boundaries over the scale domain. If provided, axes and legends will use the bin boundaries to inform the choice of tick marks and text labels. (type = array)
#' @param clamp If `true`, values that exceed the data domain are clamped to either the minimum or maximum range value
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `clamp` (`true` by default). (type = boolean)
#' @param constant A constant determining the slope of the symlog function around zero. Only used for `symlog` scales.
#' 
#' __Default value:__ `1` (type = number)
#' @param domain Customized domain values.
#' 
#' For _quantitative_ fields, `domain` can take the form of a two-element array with minimum and maximum values.  [Piecewise scales](https://vega.github.io/vega-lite/docs/scale.html#piecewise) can be created by providing a `domain` with more than two entries.
#' If the input field is aggregated, `domain` can also be a string value `"unaggregated"`, indicating that the domain should include the raw data values prior to the aggregation.
#' 
#' For _temporal_ fields, `domain` can be a two-element array minimum and maximum values, in the form of either timestamps or the [DateTime definition objects](https://vega.github.io/vega-lite/docs/types.html#datetime).
#' 
#' For _ordinal_ and _nominal_ fields, `domain` can be an array that lists valid input values.
#' 
#' The `selection` property can be used to [interactively determine](https://vega.github.io/vega-lite/docs/selection.html#scale-domains) the scale domain. (type = Varies)
#' @param exponent The exponent of the `pow` scale. (type = number)
#' @param interpolate The interpolation method for range values. By default, a general interpolator for numbers, dates, strings and colors (in HCL space) is used. For color ranges, this property allows interpolation in alternative color spaces. Legal values include `rgb`, `hsl`, `hsl-long`, `lab`, `hcl`, `hcl-long`, `cubehelix` and `cubehelix-long` ('-long' variants use longer paths in polar coordinate spaces). If object-valued, this property accepts an object with a string-valued _type_ property and an optional numeric _gamma_ property applicable to rgb and cubehelix interpolators. For more, see the [d3-interpolate documentation](https://github.com/d3/d3-interpolate).
#' 
#' * __Default value:__ `hcl` (type = Varies)
#' @param nice Extending the domain so that it starts and ends on nice round values. This method typically modifies the scale’s domain, and may only extend the bounds to the nearest round value. Nicing is useful if the domain is computed from data and may be irregular. For example, for a domain of _[0.201479…, 0.996679…]_, a nice domain might be _[0.2, 1.0]_.
#' 
#' For quantitative scales such as linear, `nice` can be either a boolean flag or a number. If `nice` is a number, it will represent a desired tick count. This allows greater control over the step size used to extend the bounds, guaranteeing that the returned ticks will exactly cover the domain.
#' 
#' For temporal fields with time and utc scales, the `nice` value can be a string indicating the desired time interval. Legal values are `"millisecond"`, `"second"`, `"minute"`, `"hour"`, `"day"`, `"week"`, `"month"`, and `"year"`. Alternatively, `time` and `utc` scales can accept an object-valued interval specifier of the form `{"interval": "month", "step": 3}`, which includes a desired number of interval steps. Here, the domain would snap to quarter (Jan, Apr, Jul, Oct) boundaries.
#' 
#' __Default value:__ `true` for unbinned _quantitative_ fields; `false` otherwise. (type = Varies)
#' @param padding For _[continuous](https://vega.github.io/vega-lite/docs/scale.html#continuous)_ scales, expands the scale domain to accommodate the specified number of pixels on each of the scale range. The scale range must represent pixels for this parameter to function as intended. Padding adjustment is performed prior to all other adjustments, including the effects of the zero, nice, domainMin, and domainMax properties.
#' 
#' For _[band](https://vega.github.io/vega-lite/docs/scale.html#band)_ scales, shortcut for setting `paddingInner` and `paddingOuter` to the same value.
#' 
#' For _[point](https://vega.github.io/vega-lite/docs/scale.html#point)_ scales, alias for `paddingOuter`.
#' 
#' __Default value:__ For _continuous_ scales, derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `continuousPadding`.
#' For _band and point_ scales, see `paddingInner` and `paddingOuter`. (type = number)
#' @param paddingInner The inner padding (spacing) within each band step of band scales, as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' For point scale, this property is invalid as point scales do not have internal band widths (only step sizes between bands).
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingInner`. (type = number)
#' @param paddingOuter The outer padding (spacing) at the ends of the range of band and point scales,
#' as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingOuter` for band scales and `pointPadding` for point scales. (type = number)
#' @param range The range of the scale. One of:
#' 
#' - A string indicating a [pre-defined named scale range](https://vega.github.io/vega-lite/docs/scale.html#range-config) (e.g., example, `"symbol"`, or `"diverging"`).
#' 
#' - For [continuous scales](https://vega.github.io/vega-lite/docs/scale.html#continuous), two-element array indicating  minimum and maximum values, or an array with more than two entries for specifying a [piecewise scale](https://vega.github.io/vega-lite/docs/scale.html#piecewise).
#' 
#' - For [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) and [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales, an array of desired output values.
#' 
#' __Notes:__
#' 
#' 1) For color scales you can also specify a color [`scheme`](https://vega.github.io/vega-lite/docs/scale.html#scheme) instead of `range`.
#' 
#' 2) Any directly specified `range` for `x` and `y` channels will be ignored. Range can be customized via the view's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` and `height`) or via [range steps and paddings properties](#range-step) for [band](#band) and [point](#point) scales. (type = Varies)
#' @param rangeStep The distance between the starts of adjacent bands or points in [band](https://vega.github.io/vega-lite/docs/scale.html#band) and [point](https://vega.github.io/vega-lite/docs/scale.html#point) scales.
#' 
#' If `rangeStep` is `null` or if the view contains the scale's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` for `x` scales and `height` for `y` scales), `rangeStep` will be automatically determined to fit the size of the view.
#' 
#' __Default value:__  derived the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `textXRangeStep` (`90` by default) for x-scales of `text` marks and `rangeStep` (`21` by default) for x-scales of other marks and y-scales.
#' 
#' __Warning__: If `rangeStep` is `null` and the cardinality of the scale's domain is higher than `width` or `height`, the rangeStep might become less than one pixel and the mark might not appear correctly. (type = number OR null)
#' @param round If `true`, rounds numeric output values to integers. This can be helpful for snapping to the pixel grid.
#' 
#' __Default value:__ `false`. (type = boolean)
#' @param scheme A string indicating a color [scheme](https://vega.github.io/vega-lite/docs/scale.html#scheme) name (e.g., `"category10"` or `"blues"`) or a [scheme parameter object](https://vega.github.io/vega-lite/docs/scale.html#scheme-params).
#' 
#' Discrete color schemes may be used with [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) or [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales. Continuous color schemes are intended for use with color scales.
#' 
#' For the full list of supported schemes, please refer to the [Vega Scheme](https://vega.github.io/vega/docs/schemes/#reference) reference. (type = Varies)
#' @param type The type of scale.  Vega-Lite supports the following categories of scale types:
#' 
#' 1) [**Continuous Scales**](https://vega.github.io/vega-lite/docs/scale.html#continuous) -- mapping continuous domains to continuous output ranges ([`"linear"`](https://vega.github.io/vega-lite/docs/scale.html#linear), [`"pow"`](https://vega.github.io/vega-lite/docs/scale.html#pow), [`"sqrt"`](https://vega.github.io/vega-lite/docs/scale.html#sqrt), [`"symlog"`](https://vega.github.io/vega-lite/docs/scale.html#symlog), [`"log"`](https://vega.github.io/vega-lite/docs/scale.html#log), [`"time"`](https://vega.github.io/vega-lite/docs/scale.html#time), [`"utc"`](https://vega.github.io/vega-lite/docs/scale.html#utc).
#' 
#' 2) [**Discrete Scales**](https://vega.github.io/vega-lite/docs/scale.html#discrete) -- mapping discrete domains to discrete ([`"ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#ordinal)) or continuous ([`"band"`](https://vega.github.io/vega-lite/docs/scale.html#band) and [`"point"`](https://vega.github.io/vega-lite/docs/scale.html#point)) output ranges.
#' 
#' 3) [**Discretizing Scales**](https://vega.github.io/vega-lite/docs/scale.html#discretizing) -- mapping continuous domains to discrete output ranges [`"bin-ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#bin-ordinal), [`"quantile"`](https://vega.github.io/vega-lite/docs/scale.html#quantile), [`"quantize"`](https://vega.github.io/vega-lite/docs/scale.html#quantize) and [`"threshold"`](https://vega.github.io/vega-lite/docs/scale.html#threshold).
#' 
#' __Default value:__ please see the [scale type table](https://vega.github.io/vega-lite/docs/scale.html#type). (type = ScaleType)
#' @param zero If `true`, ensures that a zero baseline value is included in the scale domain.
#' 
#' __Default value:__ `true` for x and y channels if the quantitative field is not binned and no custom `domain` is provided; `false` otherwise.
#' 
#' __Note:__ Log, time, and utc scales do not support `zero`. (type = boolean)
#' @return A modified spec
#' @export
vl_scale_shape <- function(spec, base = NULL, bins = NULL, clamp = NULL, constant = NULL, domain = NULL, exponent = NULL, interpolate = NULL, nice = NULL, padding = NULL, paddingInner = NULL, paddingOuter = NULL, range = NULL, rangeStep = NULL, round = NULL, scheme = NULL, type = NULL, zero = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'shape'))
  rlang::exec(.add_scale_to_encoding, !!!args_out)
} #' vl_scale_size
#' 
#' Add scale to size encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param base The logarithm base of the `log` scale (default `10`). (type = number)
#' @param bins An array of bin boundaries over the scale domain. If provided, axes and legends will use the bin boundaries to inform the choice of tick marks and text labels. (type = array)
#' @param clamp If `true`, values that exceed the data domain are clamped to either the minimum or maximum range value
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `clamp` (`true` by default). (type = boolean)
#' @param constant A constant determining the slope of the symlog function around zero. Only used for `symlog` scales.
#' 
#' __Default value:__ `1` (type = number)
#' @param domain Customized domain values.
#' 
#' For _quantitative_ fields, `domain` can take the form of a two-element array with minimum and maximum values.  [Piecewise scales](https://vega.github.io/vega-lite/docs/scale.html#piecewise) can be created by providing a `domain` with more than two entries.
#' If the input field is aggregated, `domain` can also be a string value `"unaggregated"`, indicating that the domain should include the raw data values prior to the aggregation.
#' 
#' For _temporal_ fields, `domain` can be a two-element array minimum and maximum values, in the form of either timestamps or the [DateTime definition objects](https://vega.github.io/vega-lite/docs/types.html#datetime).
#' 
#' For _ordinal_ and _nominal_ fields, `domain` can be an array that lists valid input values.
#' 
#' The `selection` property can be used to [interactively determine](https://vega.github.io/vega-lite/docs/selection.html#scale-domains) the scale domain. (type = Varies)
#' @param exponent The exponent of the `pow` scale. (type = number)
#' @param interpolate The interpolation method for range values. By default, a general interpolator for numbers, dates, strings and colors (in HCL space) is used. For color ranges, this property allows interpolation in alternative color spaces. Legal values include `rgb`, `hsl`, `hsl-long`, `lab`, `hcl`, `hcl-long`, `cubehelix` and `cubehelix-long` ('-long' variants use longer paths in polar coordinate spaces). If object-valued, this property accepts an object with a string-valued _type_ property and an optional numeric _gamma_ property applicable to rgb and cubehelix interpolators. For more, see the [d3-interpolate documentation](https://github.com/d3/d3-interpolate).
#' 
#' * __Default value:__ `hcl` (type = Varies)
#' @param nice Extending the domain so that it starts and ends on nice round values. This method typically modifies the scale’s domain, and may only extend the bounds to the nearest round value. Nicing is useful if the domain is computed from data and may be irregular. For example, for a domain of _[0.201479…, 0.996679…]_, a nice domain might be _[0.2, 1.0]_.
#' 
#' For quantitative scales such as linear, `nice` can be either a boolean flag or a number. If `nice` is a number, it will represent a desired tick count. This allows greater control over the step size used to extend the bounds, guaranteeing that the returned ticks will exactly cover the domain.
#' 
#' For temporal fields with time and utc scales, the `nice` value can be a string indicating the desired time interval. Legal values are `"millisecond"`, `"second"`, `"minute"`, `"hour"`, `"day"`, `"week"`, `"month"`, and `"year"`. Alternatively, `time` and `utc` scales can accept an object-valued interval specifier of the form `{"interval": "month", "step": 3}`, which includes a desired number of interval steps. Here, the domain would snap to quarter (Jan, Apr, Jul, Oct) boundaries.
#' 
#' __Default value:__ `true` for unbinned _quantitative_ fields; `false` otherwise. (type = Varies)
#' @param padding For _[continuous](https://vega.github.io/vega-lite/docs/scale.html#continuous)_ scales, expands the scale domain to accommodate the specified number of pixels on each of the scale range. The scale range must represent pixels for this parameter to function as intended. Padding adjustment is performed prior to all other adjustments, including the effects of the zero, nice, domainMin, and domainMax properties.
#' 
#' For _[band](https://vega.github.io/vega-lite/docs/scale.html#band)_ scales, shortcut for setting `paddingInner` and `paddingOuter` to the same value.
#' 
#' For _[point](https://vega.github.io/vega-lite/docs/scale.html#point)_ scales, alias for `paddingOuter`.
#' 
#' __Default value:__ For _continuous_ scales, derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `continuousPadding`.
#' For _band and point_ scales, see `paddingInner` and `paddingOuter`. (type = number)
#' @param paddingInner The inner padding (spacing) within each band step of band scales, as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' For point scale, this property is invalid as point scales do not have internal band widths (only step sizes between bands).
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingInner`. (type = number)
#' @param paddingOuter The outer padding (spacing) at the ends of the range of band and point scales,
#' as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingOuter` for band scales and `pointPadding` for point scales. (type = number)
#' @param range The range of the scale. One of:
#' 
#' - A string indicating a [pre-defined named scale range](https://vega.github.io/vega-lite/docs/scale.html#range-config) (e.g., example, `"symbol"`, or `"diverging"`).
#' 
#' - For [continuous scales](https://vega.github.io/vega-lite/docs/scale.html#continuous), two-element array indicating  minimum and maximum values, or an array with more than two entries for specifying a [piecewise scale](https://vega.github.io/vega-lite/docs/scale.html#piecewise).
#' 
#' - For [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) and [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales, an array of desired output values.
#' 
#' __Notes:__
#' 
#' 1) For color scales you can also specify a color [`scheme`](https://vega.github.io/vega-lite/docs/scale.html#scheme) instead of `range`.
#' 
#' 2) Any directly specified `range` for `x` and `y` channels will be ignored. Range can be customized via the view's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` and `height`) or via [range steps and paddings properties](#range-step) for [band](#band) and [point](#point) scales. (type = Varies)
#' @param rangeStep The distance between the starts of adjacent bands or points in [band](https://vega.github.io/vega-lite/docs/scale.html#band) and [point](https://vega.github.io/vega-lite/docs/scale.html#point) scales.
#' 
#' If `rangeStep` is `null` or if the view contains the scale's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` for `x` scales and `height` for `y` scales), `rangeStep` will be automatically determined to fit the size of the view.
#' 
#' __Default value:__  derived the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `textXRangeStep` (`90` by default) for x-scales of `text` marks and `rangeStep` (`21` by default) for x-scales of other marks and y-scales.
#' 
#' __Warning__: If `rangeStep` is `null` and the cardinality of the scale's domain is higher than `width` or `height`, the rangeStep might become less than one pixel and the mark might not appear correctly. (type = number OR null)
#' @param round If `true`, rounds numeric output values to integers. This can be helpful for snapping to the pixel grid.
#' 
#' __Default value:__ `false`. (type = boolean)
#' @param scheme A string indicating a color [scheme](https://vega.github.io/vega-lite/docs/scale.html#scheme) name (e.g., `"category10"` or `"blues"`) or a [scheme parameter object](https://vega.github.io/vega-lite/docs/scale.html#scheme-params).
#' 
#' Discrete color schemes may be used with [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) or [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales. Continuous color schemes are intended for use with color scales.
#' 
#' For the full list of supported schemes, please refer to the [Vega Scheme](https://vega.github.io/vega/docs/schemes/#reference) reference. (type = Varies)
#' @param type The type of scale.  Vega-Lite supports the following categories of scale types:
#' 
#' 1) [**Continuous Scales**](https://vega.github.io/vega-lite/docs/scale.html#continuous) -- mapping continuous domains to continuous output ranges ([`"linear"`](https://vega.github.io/vega-lite/docs/scale.html#linear), [`"pow"`](https://vega.github.io/vega-lite/docs/scale.html#pow), [`"sqrt"`](https://vega.github.io/vega-lite/docs/scale.html#sqrt), [`"symlog"`](https://vega.github.io/vega-lite/docs/scale.html#symlog), [`"log"`](https://vega.github.io/vega-lite/docs/scale.html#log), [`"time"`](https://vega.github.io/vega-lite/docs/scale.html#time), [`"utc"`](https://vega.github.io/vega-lite/docs/scale.html#utc).
#' 
#' 2) [**Discrete Scales**](https://vega.github.io/vega-lite/docs/scale.html#discrete) -- mapping discrete domains to discrete ([`"ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#ordinal)) or continuous ([`"band"`](https://vega.github.io/vega-lite/docs/scale.html#band) and [`"point"`](https://vega.github.io/vega-lite/docs/scale.html#point)) output ranges.
#' 
#' 3) [**Discretizing Scales**](https://vega.github.io/vega-lite/docs/scale.html#discretizing) -- mapping continuous domains to discrete output ranges [`"bin-ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#bin-ordinal), [`"quantile"`](https://vega.github.io/vega-lite/docs/scale.html#quantile), [`"quantize"`](https://vega.github.io/vega-lite/docs/scale.html#quantize) and [`"threshold"`](https://vega.github.io/vega-lite/docs/scale.html#threshold).
#' 
#' __Default value:__ please see the [scale type table](https://vega.github.io/vega-lite/docs/scale.html#type). (type = ScaleType)
#' @param zero If `true`, ensures that a zero baseline value is included in the scale domain.
#' 
#' __Default value:__ `true` for x and y channels if the quantitative field is not binned and no custom `domain` is provided; `false` otherwise.
#' 
#' __Note:__ Log, time, and utc scales do not support `zero`. (type = boolean)
#' @return A modified spec
#' @export
vl_scale_size <- function(spec, base = NULL, bins = NULL, clamp = NULL, constant = NULL, domain = NULL, exponent = NULL, interpolate = NULL, nice = NULL, padding = NULL, paddingInner = NULL, paddingOuter = NULL, range = NULL, rangeStep = NULL, round = NULL, scheme = NULL, type = NULL, zero = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'size'))
  rlang::exec(.add_scale_to_encoding, !!!args_out)
} #' vl_scale_stroke
#' 
#' Add scale to stroke encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param base The logarithm base of the `log` scale (default `10`). (type = number)
#' @param bins An array of bin boundaries over the scale domain. If provided, axes and legends will use the bin boundaries to inform the choice of tick marks and text labels. (type = array)
#' @param clamp If `true`, values that exceed the data domain are clamped to either the minimum or maximum range value
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `clamp` (`true` by default). (type = boolean)
#' @param constant A constant determining the slope of the symlog function around zero. Only used for `symlog` scales.
#' 
#' __Default value:__ `1` (type = number)
#' @param domain Customized domain values.
#' 
#' For _quantitative_ fields, `domain` can take the form of a two-element array with minimum and maximum values.  [Piecewise scales](https://vega.github.io/vega-lite/docs/scale.html#piecewise) can be created by providing a `domain` with more than two entries.
#' If the input field is aggregated, `domain` can also be a string value `"unaggregated"`, indicating that the domain should include the raw data values prior to the aggregation.
#' 
#' For _temporal_ fields, `domain` can be a two-element array minimum and maximum values, in the form of either timestamps or the [DateTime definition objects](https://vega.github.io/vega-lite/docs/types.html#datetime).
#' 
#' For _ordinal_ and _nominal_ fields, `domain` can be an array that lists valid input values.
#' 
#' The `selection` property can be used to [interactively determine](https://vega.github.io/vega-lite/docs/selection.html#scale-domains) the scale domain. (type = Varies)
#' @param exponent The exponent of the `pow` scale. (type = number)
#' @param interpolate The interpolation method for range values. By default, a general interpolator for numbers, dates, strings and colors (in HCL space) is used. For color ranges, this property allows interpolation in alternative color spaces. Legal values include `rgb`, `hsl`, `hsl-long`, `lab`, `hcl`, `hcl-long`, `cubehelix` and `cubehelix-long` ('-long' variants use longer paths in polar coordinate spaces). If object-valued, this property accepts an object with a string-valued _type_ property and an optional numeric _gamma_ property applicable to rgb and cubehelix interpolators. For more, see the [d3-interpolate documentation](https://github.com/d3/d3-interpolate).
#' 
#' * __Default value:__ `hcl` (type = Varies)
#' @param nice Extending the domain so that it starts and ends on nice round values. This method typically modifies the scale’s domain, and may only extend the bounds to the nearest round value. Nicing is useful if the domain is computed from data and may be irregular. For example, for a domain of _[0.201479…, 0.996679…]_, a nice domain might be _[0.2, 1.0]_.
#' 
#' For quantitative scales such as linear, `nice` can be either a boolean flag or a number. If `nice` is a number, it will represent a desired tick count. This allows greater control over the step size used to extend the bounds, guaranteeing that the returned ticks will exactly cover the domain.
#' 
#' For temporal fields with time and utc scales, the `nice` value can be a string indicating the desired time interval. Legal values are `"millisecond"`, `"second"`, `"minute"`, `"hour"`, `"day"`, `"week"`, `"month"`, and `"year"`. Alternatively, `time` and `utc` scales can accept an object-valued interval specifier of the form `{"interval": "month", "step": 3}`, which includes a desired number of interval steps. Here, the domain would snap to quarter (Jan, Apr, Jul, Oct) boundaries.
#' 
#' __Default value:__ `true` for unbinned _quantitative_ fields; `false` otherwise. (type = Varies)
#' @param padding For _[continuous](https://vega.github.io/vega-lite/docs/scale.html#continuous)_ scales, expands the scale domain to accommodate the specified number of pixels on each of the scale range. The scale range must represent pixels for this parameter to function as intended. Padding adjustment is performed prior to all other adjustments, including the effects of the zero, nice, domainMin, and domainMax properties.
#' 
#' For _[band](https://vega.github.io/vega-lite/docs/scale.html#band)_ scales, shortcut for setting `paddingInner` and `paddingOuter` to the same value.
#' 
#' For _[point](https://vega.github.io/vega-lite/docs/scale.html#point)_ scales, alias for `paddingOuter`.
#' 
#' __Default value:__ For _continuous_ scales, derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `continuousPadding`.
#' For _band and point_ scales, see `paddingInner` and `paddingOuter`. (type = number)
#' @param paddingInner The inner padding (spacing) within each band step of band scales, as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' For point scale, this property is invalid as point scales do not have internal band widths (only step sizes between bands).
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingInner`. (type = number)
#' @param paddingOuter The outer padding (spacing) at the ends of the range of band and point scales,
#' as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingOuter` for band scales and `pointPadding` for point scales. (type = number)
#' @param range The range of the scale. One of:
#' 
#' - A string indicating a [pre-defined named scale range](https://vega.github.io/vega-lite/docs/scale.html#range-config) (e.g., example, `"symbol"`, or `"diverging"`).
#' 
#' - For [continuous scales](https://vega.github.io/vega-lite/docs/scale.html#continuous), two-element array indicating  minimum and maximum values, or an array with more than two entries for specifying a [piecewise scale](https://vega.github.io/vega-lite/docs/scale.html#piecewise).
#' 
#' - For [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) and [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales, an array of desired output values.
#' 
#' __Notes:__
#' 
#' 1) For color scales you can also specify a color [`scheme`](https://vega.github.io/vega-lite/docs/scale.html#scheme) instead of `range`.
#' 
#' 2) Any directly specified `range` for `x` and `y` channels will be ignored. Range can be customized via the view's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` and `height`) or via [range steps and paddings properties](#range-step) for [band](#band) and [point](#point) scales. (type = Varies)
#' @param rangeStep The distance between the starts of adjacent bands or points in [band](https://vega.github.io/vega-lite/docs/scale.html#band) and [point](https://vega.github.io/vega-lite/docs/scale.html#point) scales.
#' 
#' If `rangeStep` is `null` or if the view contains the scale's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` for `x` scales and `height` for `y` scales), `rangeStep` will be automatically determined to fit the size of the view.
#' 
#' __Default value:__  derived the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `textXRangeStep` (`90` by default) for x-scales of `text` marks and `rangeStep` (`21` by default) for x-scales of other marks and y-scales.
#' 
#' __Warning__: If `rangeStep` is `null` and the cardinality of the scale's domain is higher than `width` or `height`, the rangeStep might become less than one pixel and the mark might not appear correctly. (type = number OR null)
#' @param round If `true`, rounds numeric output values to integers. This can be helpful for snapping to the pixel grid.
#' 
#' __Default value:__ `false`. (type = boolean)
#' @param scheme A string indicating a color [scheme](https://vega.github.io/vega-lite/docs/scale.html#scheme) name (e.g., `"category10"` or `"blues"`) or a [scheme parameter object](https://vega.github.io/vega-lite/docs/scale.html#scheme-params).
#' 
#' Discrete color schemes may be used with [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) or [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales. Continuous color schemes are intended for use with color scales.
#' 
#' For the full list of supported schemes, please refer to the [Vega Scheme](https://vega.github.io/vega/docs/schemes/#reference) reference. (type = Varies)
#' @param type The type of scale.  Vega-Lite supports the following categories of scale types:
#' 
#' 1) [**Continuous Scales**](https://vega.github.io/vega-lite/docs/scale.html#continuous) -- mapping continuous domains to continuous output ranges ([`"linear"`](https://vega.github.io/vega-lite/docs/scale.html#linear), [`"pow"`](https://vega.github.io/vega-lite/docs/scale.html#pow), [`"sqrt"`](https://vega.github.io/vega-lite/docs/scale.html#sqrt), [`"symlog"`](https://vega.github.io/vega-lite/docs/scale.html#symlog), [`"log"`](https://vega.github.io/vega-lite/docs/scale.html#log), [`"time"`](https://vega.github.io/vega-lite/docs/scale.html#time), [`"utc"`](https://vega.github.io/vega-lite/docs/scale.html#utc).
#' 
#' 2) [**Discrete Scales**](https://vega.github.io/vega-lite/docs/scale.html#discrete) -- mapping discrete domains to discrete ([`"ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#ordinal)) or continuous ([`"band"`](https://vega.github.io/vega-lite/docs/scale.html#band) and [`"point"`](https://vega.github.io/vega-lite/docs/scale.html#point)) output ranges.
#' 
#' 3) [**Discretizing Scales**](https://vega.github.io/vega-lite/docs/scale.html#discretizing) -- mapping continuous domains to discrete output ranges [`"bin-ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#bin-ordinal), [`"quantile"`](https://vega.github.io/vega-lite/docs/scale.html#quantile), [`"quantize"`](https://vega.github.io/vega-lite/docs/scale.html#quantize) and [`"threshold"`](https://vega.github.io/vega-lite/docs/scale.html#threshold).
#' 
#' __Default value:__ please see the [scale type table](https://vega.github.io/vega-lite/docs/scale.html#type). (type = ScaleType)
#' @param zero If `true`, ensures that a zero baseline value is included in the scale domain.
#' 
#' __Default value:__ `true` for x and y channels if the quantitative field is not binned and no custom `domain` is provided; `false` otherwise.
#' 
#' __Note:__ Log, time, and utc scales do not support `zero`. (type = boolean)
#' @return A modified spec
#' @export
vl_scale_stroke <- function(spec, base = NULL, bins = NULL, clamp = NULL, constant = NULL, domain = NULL, exponent = NULL, interpolate = NULL, nice = NULL, padding = NULL, paddingInner = NULL, paddingOuter = NULL, range = NULL, rangeStep = NULL, round = NULL, scheme = NULL, type = NULL, zero = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'stroke'))
  rlang::exec(.add_scale_to_encoding, !!!args_out)
} #' vl_scale_strokeOpacity
#' 
#' Add scale to strokeOpacity encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param base The logarithm base of the `log` scale (default `10`). (type = number)
#' @param bins An array of bin boundaries over the scale domain. If provided, axes and legends will use the bin boundaries to inform the choice of tick marks and text labels. (type = array)
#' @param clamp If `true`, values that exceed the data domain are clamped to either the minimum or maximum range value
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `clamp` (`true` by default). (type = boolean)
#' @param constant A constant determining the slope of the symlog function around zero. Only used for `symlog` scales.
#' 
#' __Default value:__ `1` (type = number)
#' @param domain Customized domain values.
#' 
#' For _quantitative_ fields, `domain` can take the form of a two-element array with minimum and maximum values.  [Piecewise scales](https://vega.github.io/vega-lite/docs/scale.html#piecewise) can be created by providing a `domain` with more than two entries.
#' If the input field is aggregated, `domain` can also be a string value `"unaggregated"`, indicating that the domain should include the raw data values prior to the aggregation.
#' 
#' For _temporal_ fields, `domain` can be a two-element array minimum and maximum values, in the form of either timestamps or the [DateTime definition objects](https://vega.github.io/vega-lite/docs/types.html#datetime).
#' 
#' For _ordinal_ and _nominal_ fields, `domain` can be an array that lists valid input values.
#' 
#' The `selection` property can be used to [interactively determine](https://vega.github.io/vega-lite/docs/selection.html#scale-domains) the scale domain. (type = Varies)
#' @param exponent The exponent of the `pow` scale. (type = number)
#' @param interpolate The interpolation method for range values. By default, a general interpolator for numbers, dates, strings and colors (in HCL space) is used. For color ranges, this property allows interpolation in alternative color spaces. Legal values include `rgb`, `hsl`, `hsl-long`, `lab`, `hcl`, `hcl-long`, `cubehelix` and `cubehelix-long` ('-long' variants use longer paths in polar coordinate spaces). If object-valued, this property accepts an object with a string-valued _type_ property and an optional numeric _gamma_ property applicable to rgb and cubehelix interpolators. For more, see the [d3-interpolate documentation](https://github.com/d3/d3-interpolate).
#' 
#' * __Default value:__ `hcl` (type = Varies)
#' @param nice Extending the domain so that it starts and ends on nice round values. This method typically modifies the scale’s domain, and may only extend the bounds to the nearest round value. Nicing is useful if the domain is computed from data and may be irregular. For example, for a domain of _[0.201479…, 0.996679…]_, a nice domain might be _[0.2, 1.0]_.
#' 
#' For quantitative scales such as linear, `nice` can be either a boolean flag or a number. If `nice` is a number, it will represent a desired tick count. This allows greater control over the step size used to extend the bounds, guaranteeing that the returned ticks will exactly cover the domain.
#' 
#' For temporal fields with time and utc scales, the `nice` value can be a string indicating the desired time interval. Legal values are `"millisecond"`, `"second"`, `"minute"`, `"hour"`, `"day"`, `"week"`, `"month"`, and `"year"`. Alternatively, `time` and `utc` scales can accept an object-valued interval specifier of the form `{"interval": "month", "step": 3}`, which includes a desired number of interval steps. Here, the domain would snap to quarter (Jan, Apr, Jul, Oct) boundaries.
#' 
#' __Default value:__ `true` for unbinned _quantitative_ fields; `false` otherwise. (type = Varies)
#' @param padding For _[continuous](https://vega.github.io/vega-lite/docs/scale.html#continuous)_ scales, expands the scale domain to accommodate the specified number of pixels on each of the scale range. The scale range must represent pixels for this parameter to function as intended. Padding adjustment is performed prior to all other adjustments, including the effects of the zero, nice, domainMin, and domainMax properties.
#' 
#' For _[band](https://vega.github.io/vega-lite/docs/scale.html#band)_ scales, shortcut for setting `paddingInner` and `paddingOuter` to the same value.
#' 
#' For _[point](https://vega.github.io/vega-lite/docs/scale.html#point)_ scales, alias for `paddingOuter`.
#' 
#' __Default value:__ For _continuous_ scales, derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `continuousPadding`.
#' For _band and point_ scales, see `paddingInner` and `paddingOuter`. (type = number)
#' @param paddingInner The inner padding (spacing) within each band step of band scales, as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' For point scale, this property is invalid as point scales do not have internal band widths (only step sizes between bands).
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingInner`. (type = number)
#' @param paddingOuter The outer padding (spacing) at the ends of the range of band and point scales,
#' as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingOuter` for band scales and `pointPadding` for point scales. (type = number)
#' @param range The range of the scale. One of:
#' 
#' - A string indicating a [pre-defined named scale range](https://vega.github.io/vega-lite/docs/scale.html#range-config) (e.g., example, `"symbol"`, or `"diverging"`).
#' 
#' - For [continuous scales](https://vega.github.io/vega-lite/docs/scale.html#continuous), two-element array indicating  minimum and maximum values, or an array with more than two entries for specifying a [piecewise scale](https://vega.github.io/vega-lite/docs/scale.html#piecewise).
#' 
#' - For [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) and [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales, an array of desired output values.
#' 
#' __Notes:__
#' 
#' 1) For color scales you can also specify a color [`scheme`](https://vega.github.io/vega-lite/docs/scale.html#scheme) instead of `range`.
#' 
#' 2) Any directly specified `range` for `x` and `y` channels will be ignored. Range can be customized via the view's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` and `height`) or via [range steps and paddings properties](#range-step) for [band](#band) and [point](#point) scales. (type = Varies)
#' @param rangeStep The distance between the starts of adjacent bands or points in [band](https://vega.github.io/vega-lite/docs/scale.html#band) and [point](https://vega.github.io/vega-lite/docs/scale.html#point) scales.
#' 
#' If `rangeStep` is `null` or if the view contains the scale's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` for `x` scales and `height` for `y` scales), `rangeStep` will be automatically determined to fit the size of the view.
#' 
#' __Default value:__  derived the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `textXRangeStep` (`90` by default) for x-scales of `text` marks and `rangeStep` (`21` by default) for x-scales of other marks and y-scales.
#' 
#' __Warning__: If `rangeStep` is `null` and the cardinality of the scale's domain is higher than `width` or `height`, the rangeStep might become less than one pixel and the mark might not appear correctly. (type = number OR null)
#' @param round If `true`, rounds numeric output values to integers. This can be helpful for snapping to the pixel grid.
#' 
#' __Default value:__ `false`. (type = boolean)
#' @param scheme A string indicating a color [scheme](https://vega.github.io/vega-lite/docs/scale.html#scheme) name (e.g., `"category10"` or `"blues"`) or a [scheme parameter object](https://vega.github.io/vega-lite/docs/scale.html#scheme-params).
#' 
#' Discrete color schemes may be used with [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) or [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales. Continuous color schemes are intended for use with color scales.
#' 
#' For the full list of supported schemes, please refer to the [Vega Scheme](https://vega.github.io/vega/docs/schemes/#reference) reference. (type = Varies)
#' @param type The type of scale.  Vega-Lite supports the following categories of scale types:
#' 
#' 1) [**Continuous Scales**](https://vega.github.io/vega-lite/docs/scale.html#continuous) -- mapping continuous domains to continuous output ranges ([`"linear"`](https://vega.github.io/vega-lite/docs/scale.html#linear), [`"pow"`](https://vega.github.io/vega-lite/docs/scale.html#pow), [`"sqrt"`](https://vega.github.io/vega-lite/docs/scale.html#sqrt), [`"symlog"`](https://vega.github.io/vega-lite/docs/scale.html#symlog), [`"log"`](https://vega.github.io/vega-lite/docs/scale.html#log), [`"time"`](https://vega.github.io/vega-lite/docs/scale.html#time), [`"utc"`](https://vega.github.io/vega-lite/docs/scale.html#utc).
#' 
#' 2) [**Discrete Scales**](https://vega.github.io/vega-lite/docs/scale.html#discrete) -- mapping discrete domains to discrete ([`"ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#ordinal)) or continuous ([`"band"`](https://vega.github.io/vega-lite/docs/scale.html#band) and [`"point"`](https://vega.github.io/vega-lite/docs/scale.html#point)) output ranges.
#' 
#' 3) [**Discretizing Scales**](https://vega.github.io/vega-lite/docs/scale.html#discretizing) -- mapping continuous domains to discrete output ranges [`"bin-ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#bin-ordinal), [`"quantile"`](https://vega.github.io/vega-lite/docs/scale.html#quantile), [`"quantize"`](https://vega.github.io/vega-lite/docs/scale.html#quantize) and [`"threshold"`](https://vega.github.io/vega-lite/docs/scale.html#threshold).
#' 
#' __Default value:__ please see the [scale type table](https://vega.github.io/vega-lite/docs/scale.html#type). (type = ScaleType)
#' @param zero If `true`, ensures that a zero baseline value is included in the scale domain.
#' 
#' __Default value:__ `true` for x and y channels if the quantitative field is not binned and no custom `domain` is provided; `false` otherwise.
#' 
#' __Note:__ Log, time, and utc scales do not support `zero`. (type = boolean)
#' @return A modified spec
#' @export
vl_scale_strokeOpacity <- function(spec, base = NULL, bins = NULL, clamp = NULL, constant = NULL, domain = NULL, exponent = NULL, interpolate = NULL, nice = NULL, padding = NULL, paddingInner = NULL, paddingOuter = NULL, range = NULL, rangeStep = NULL, round = NULL, scheme = NULL, type = NULL, zero = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'strokeOpacity'))
  rlang::exec(.add_scale_to_encoding, !!!args_out)
} #' vl_scale_strokeWidth
#' 
#' Add scale to strokeWidth encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param base The logarithm base of the `log` scale (default `10`). (type = number)
#' @param bins An array of bin boundaries over the scale domain. If provided, axes and legends will use the bin boundaries to inform the choice of tick marks and text labels. (type = array)
#' @param clamp If `true`, values that exceed the data domain are clamped to either the minimum or maximum range value
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `clamp` (`true` by default). (type = boolean)
#' @param constant A constant determining the slope of the symlog function around zero. Only used for `symlog` scales.
#' 
#' __Default value:__ `1` (type = number)
#' @param domain Customized domain values.
#' 
#' For _quantitative_ fields, `domain` can take the form of a two-element array with minimum and maximum values.  [Piecewise scales](https://vega.github.io/vega-lite/docs/scale.html#piecewise) can be created by providing a `domain` with more than two entries.
#' If the input field is aggregated, `domain` can also be a string value `"unaggregated"`, indicating that the domain should include the raw data values prior to the aggregation.
#' 
#' For _temporal_ fields, `domain` can be a two-element array minimum and maximum values, in the form of either timestamps or the [DateTime definition objects](https://vega.github.io/vega-lite/docs/types.html#datetime).
#' 
#' For _ordinal_ and _nominal_ fields, `domain` can be an array that lists valid input values.
#' 
#' The `selection` property can be used to [interactively determine](https://vega.github.io/vega-lite/docs/selection.html#scale-domains) the scale domain. (type = Varies)
#' @param exponent The exponent of the `pow` scale. (type = number)
#' @param interpolate The interpolation method for range values. By default, a general interpolator for numbers, dates, strings and colors (in HCL space) is used. For color ranges, this property allows interpolation in alternative color spaces. Legal values include `rgb`, `hsl`, `hsl-long`, `lab`, `hcl`, `hcl-long`, `cubehelix` and `cubehelix-long` ('-long' variants use longer paths in polar coordinate spaces). If object-valued, this property accepts an object with a string-valued _type_ property and an optional numeric _gamma_ property applicable to rgb and cubehelix interpolators. For more, see the [d3-interpolate documentation](https://github.com/d3/d3-interpolate).
#' 
#' * __Default value:__ `hcl` (type = Varies)
#' @param nice Extending the domain so that it starts and ends on nice round values. This method typically modifies the scale’s domain, and may only extend the bounds to the nearest round value. Nicing is useful if the domain is computed from data and may be irregular. For example, for a domain of _[0.201479…, 0.996679…]_, a nice domain might be _[0.2, 1.0]_.
#' 
#' For quantitative scales such as linear, `nice` can be either a boolean flag or a number. If `nice` is a number, it will represent a desired tick count. This allows greater control over the step size used to extend the bounds, guaranteeing that the returned ticks will exactly cover the domain.
#' 
#' For temporal fields with time and utc scales, the `nice` value can be a string indicating the desired time interval. Legal values are `"millisecond"`, `"second"`, `"minute"`, `"hour"`, `"day"`, `"week"`, `"month"`, and `"year"`. Alternatively, `time` and `utc` scales can accept an object-valued interval specifier of the form `{"interval": "month", "step": 3}`, which includes a desired number of interval steps. Here, the domain would snap to quarter (Jan, Apr, Jul, Oct) boundaries.
#' 
#' __Default value:__ `true` for unbinned _quantitative_ fields; `false` otherwise. (type = Varies)
#' @param padding For _[continuous](https://vega.github.io/vega-lite/docs/scale.html#continuous)_ scales, expands the scale domain to accommodate the specified number of pixels on each of the scale range. The scale range must represent pixels for this parameter to function as intended. Padding adjustment is performed prior to all other adjustments, including the effects of the zero, nice, domainMin, and domainMax properties.
#' 
#' For _[band](https://vega.github.io/vega-lite/docs/scale.html#band)_ scales, shortcut for setting `paddingInner` and `paddingOuter` to the same value.
#' 
#' For _[point](https://vega.github.io/vega-lite/docs/scale.html#point)_ scales, alias for `paddingOuter`.
#' 
#' __Default value:__ For _continuous_ scales, derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `continuousPadding`.
#' For _band and point_ scales, see `paddingInner` and `paddingOuter`. (type = number)
#' @param paddingInner The inner padding (spacing) within each band step of band scales, as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' For point scale, this property is invalid as point scales do not have internal band widths (only step sizes between bands).
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingInner`. (type = number)
#' @param paddingOuter The outer padding (spacing) at the ends of the range of band and point scales,
#' as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingOuter` for band scales and `pointPadding` for point scales. (type = number)
#' @param range The range of the scale. One of:
#' 
#' - A string indicating a [pre-defined named scale range](https://vega.github.io/vega-lite/docs/scale.html#range-config) (e.g., example, `"symbol"`, or `"diverging"`).
#' 
#' - For [continuous scales](https://vega.github.io/vega-lite/docs/scale.html#continuous), two-element array indicating  minimum and maximum values, or an array with more than two entries for specifying a [piecewise scale](https://vega.github.io/vega-lite/docs/scale.html#piecewise).
#' 
#' - For [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) and [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales, an array of desired output values.
#' 
#' __Notes:__
#' 
#' 1) For color scales you can also specify a color [`scheme`](https://vega.github.io/vega-lite/docs/scale.html#scheme) instead of `range`.
#' 
#' 2) Any directly specified `range` for `x` and `y` channels will be ignored. Range can be customized via the view's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` and `height`) or via [range steps and paddings properties](#range-step) for [band](#band) and [point](#point) scales. (type = Varies)
#' @param rangeStep The distance between the starts of adjacent bands or points in [band](https://vega.github.io/vega-lite/docs/scale.html#band) and [point](https://vega.github.io/vega-lite/docs/scale.html#point) scales.
#' 
#' If `rangeStep` is `null` or if the view contains the scale's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` for `x` scales and `height` for `y` scales), `rangeStep` will be automatically determined to fit the size of the view.
#' 
#' __Default value:__  derived the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `textXRangeStep` (`90` by default) for x-scales of `text` marks and `rangeStep` (`21` by default) for x-scales of other marks and y-scales.
#' 
#' __Warning__: If `rangeStep` is `null` and the cardinality of the scale's domain is higher than `width` or `height`, the rangeStep might become less than one pixel and the mark might not appear correctly. (type = number OR null)
#' @param round If `true`, rounds numeric output values to integers. This can be helpful for snapping to the pixel grid.
#' 
#' __Default value:__ `false`. (type = boolean)
#' @param scheme A string indicating a color [scheme](https://vega.github.io/vega-lite/docs/scale.html#scheme) name (e.g., `"category10"` or `"blues"`) or a [scheme parameter object](https://vega.github.io/vega-lite/docs/scale.html#scheme-params).
#' 
#' Discrete color schemes may be used with [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) or [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales. Continuous color schemes are intended for use with color scales.
#' 
#' For the full list of supported schemes, please refer to the [Vega Scheme](https://vega.github.io/vega/docs/schemes/#reference) reference. (type = Varies)
#' @param type The type of scale.  Vega-Lite supports the following categories of scale types:
#' 
#' 1) [**Continuous Scales**](https://vega.github.io/vega-lite/docs/scale.html#continuous) -- mapping continuous domains to continuous output ranges ([`"linear"`](https://vega.github.io/vega-lite/docs/scale.html#linear), [`"pow"`](https://vega.github.io/vega-lite/docs/scale.html#pow), [`"sqrt"`](https://vega.github.io/vega-lite/docs/scale.html#sqrt), [`"symlog"`](https://vega.github.io/vega-lite/docs/scale.html#symlog), [`"log"`](https://vega.github.io/vega-lite/docs/scale.html#log), [`"time"`](https://vega.github.io/vega-lite/docs/scale.html#time), [`"utc"`](https://vega.github.io/vega-lite/docs/scale.html#utc).
#' 
#' 2) [**Discrete Scales**](https://vega.github.io/vega-lite/docs/scale.html#discrete) -- mapping discrete domains to discrete ([`"ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#ordinal)) or continuous ([`"band"`](https://vega.github.io/vega-lite/docs/scale.html#band) and [`"point"`](https://vega.github.io/vega-lite/docs/scale.html#point)) output ranges.
#' 
#' 3) [**Discretizing Scales**](https://vega.github.io/vega-lite/docs/scale.html#discretizing) -- mapping continuous domains to discrete output ranges [`"bin-ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#bin-ordinal), [`"quantile"`](https://vega.github.io/vega-lite/docs/scale.html#quantile), [`"quantize"`](https://vega.github.io/vega-lite/docs/scale.html#quantize) and [`"threshold"`](https://vega.github.io/vega-lite/docs/scale.html#threshold).
#' 
#' __Default value:__ please see the [scale type table](https://vega.github.io/vega-lite/docs/scale.html#type). (type = ScaleType)
#' @param zero If `true`, ensures that a zero baseline value is included in the scale domain.
#' 
#' __Default value:__ `true` for x and y channels if the quantitative field is not binned and no custom `domain` is provided; `false` otherwise.
#' 
#' __Note:__ Log, time, and utc scales do not support `zero`. (type = boolean)
#' @return A modified spec
#' @export
vl_scale_strokeWidth <- function(spec, base = NULL, bins = NULL, clamp = NULL, constant = NULL, domain = NULL, exponent = NULL, interpolate = NULL, nice = NULL, padding = NULL, paddingInner = NULL, paddingOuter = NULL, range = NULL, rangeStep = NULL, round = NULL, scheme = NULL, type = NULL, zero = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'strokeWidth'))
  rlang::exec(.add_scale_to_encoding, !!!args_out)
} #' vl_scale_x
#' 
#' Add scale to x encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param base The logarithm base of the `log` scale (default `10`). (type = number)
#' @param bins An array of bin boundaries over the scale domain. If provided, axes and legends will use the bin boundaries to inform the choice of tick marks and text labels. (type = array)
#' @param clamp If `true`, values that exceed the data domain are clamped to either the minimum or maximum range value
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `clamp` (`true` by default). (type = boolean)
#' @param constant A constant determining the slope of the symlog function around zero. Only used for `symlog` scales.
#' 
#' __Default value:__ `1` (type = number)
#' @param domain Customized domain values.
#' 
#' For _quantitative_ fields, `domain` can take the form of a two-element array with minimum and maximum values.  [Piecewise scales](https://vega.github.io/vega-lite/docs/scale.html#piecewise) can be created by providing a `domain` with more than two entries.
#' If the input field is aggregated, `domain` can also be a string value `"unaggregated"`, indicating that the domain should include the raw data values prior to the aggregation.
#' 
#' For _temporal_ fields, `domain` can be a two-element array minimum and maximum values, in the form of either timestamps or the [DateTime definition objects](https://vega.github.io/vega-lite/docs/types.html#datetime).
#' 
#' For _ordinal_ and _nominal_ fields, `domain` can be an array that lists valid input values.
#' 
#' The `selection` property can be used to [interactively determine](https://vega.github.io/vega-lite/docs/selection.html#scale-domains) the scale domain. (type = Varies)
#' @param exponent The exponent of the `pow` scale. (type = number)
#' @param interpolate The interpolation method for range values. By default, a general interpolator for numbers, dates, strings and colors (in HCL space) is used. For color ranges, this property allows interpolation in alternative color spaces. Legal values include `rgb`, `hsl`, `hsl-long`, `lab`, `hcl`, `hcl-long`, `cubehelix` and `cubehelix-long` ('-long' variants use longer paths in polar coordinate spaces). If object-valued, this property accepts an object with a string-valued _type_ property and an optional numeric _gamma_ property applicable to rgb and cubehelix interpolators. For more, see the [d3-interpolate documentation](https://github.com/d3/d3-interpolate).
#' 
#' * __Default value:__ `hcl` (type = Varies)
#' @param nice Extending the domain so that it starts and ends on nice round values. This method typically modifies the scale’s domain, and may only extend the bounds to the nearest round value. Nicing is useful if the domain is computed from data and may be irregular. For example, for a domain of _[0.201479…, 0.996679…]_, a nice domain might be _[0.2, 1.0]_.
#' 
#' For quantitative scales such as linear, `nice` can be either a boolean flag or a number. If `nice` is a number, it will represent a desired tick count. This allows greater control over the step size used to extend the bounds, guaranteeing that the returned ticks will exactly cover the domain.
#' 
#' For temporal fields with time and utc scales, the `nice` value can be a string indicating the desired time interval. Legal values are `"millisecond"`, `"second"`, `"minute"`, `"hour"`, `"day"`, `"week"`, `"month"`, and `"year"`. Alternatively, `time` and `utc` scales can accept an object-valued interval specifier of the form `{"interval": "month", "step": 3}`, which includes a desired number of interval steps. Here, the domain would snap to quarter (Jan, Apr, Jul, Oct) boundaries.
#' 
#' __Default value:__ `true` for unbinned _quantitative_ fields; `false` otherwise. (type = Varies)
#' @param padding For _[continuous](https://vega.github.io/vega-lite/docs/scale.html#continuous)_ scales, expands the scale domain to accommodate the specified number of pixels on each of the scale range. The scale range must represent pixels for this parameter to function as intended. Padding adjustment is performed prior to all other adjustments, including the effects of the zero, nice, domainMin, and domainMax properties.
#' 
#' For _[band](https://vega.github.io/vega-lite/docs/scale.html#band)_ scales, shortcut for setting `paddingInner` and `paddingOuter` to the same value.
#' 
#' For _[point](https://vega.github.io/vega-lite/docs/scale.html#point)_ scales, alias for `paddingOuter`.
#' 
#' __Default value:__ For _continuous_ scales, derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `continuousPadding`.
#' For _band and point_ scales, see `paddingInner` and `paddingOuter`. (type = number)
#' @param paddingInner The inner padding (spacing) within each band step of band scales, as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' For point scale, this property is invalid as point scales do not have internal band widths (only step sizes between bands).
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingInner`. (type = number)
#' @param paddingOuter The outer padding (spacing) at the ends of the range of band and point scales,
#' as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingOuter` for band scales and `pointPadding` for point scales. (type = number)
#' @param range The range of the scale. One of:
#' 
#' - A string indicating a [pre-defined named scale range](https://vega.github.io/vega-lite/docs/scale.html#range-config) (e.g., example, `"symbol"`, or `"diverging"`).
#' 
#' - For [continuous scales](https://vega.github.io/vega-lite/docs/scale.html#continuous), two-element array indicating  minimum and maximum values, or an array with more than two entries for specifying a [piecewise scale](https://vega.github.io/vega-lite/docs/scale.html#piecewise).
#' 
#' - For [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) and [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales, an array of desired output values.
#' 
#' __Notes:__
#' 
#' 1) For color scales you can also specify a color [`scheme`](https://vega.github.io/vega-lite/docs/scale.html#scheme) instead of `range`.
#' 
#' 2) Any directly specified `range` for `x` and `y` channels will be ignored. Range can be customized via the view's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` and `height`) or via [range steps and paddings properties](#range-step) for [band](#band) and [point](#point) scales. (type = Varies)
#' @param rangeStep The distance between the starts of adjacent bands or points in [band](https://vega.github.io/vega-lite/docs/scale.html#band) and [point](https://vega.github.io/vega-lite/docs/scale.html#point) scales.
#' 
#' If `rangeStep` is `null` or if the view contains the scale's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` for `x` scales and `height` for `y` scales), `rangeStep` will be automatically determined to fit the size of the view.
#' 
#' __Default value:__  derived the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `textXRangeStep` (`90` by default) for x-scales of `text` marks and `rangeStep` (`21` by default) for x-scales of other marks and y-scales.
#' 
#' __Warning__: If `rangeStep` is `null` and the cardinality of the scale's domain is higher than `width` or `height`, the rangeStep might become less than one pixel and the mark might not appear correctly. (type = number OR null)
#' @param round If `true`, rounds numeric output values to integers. This can be helpful for snapping to the pixel grid.
#' 
#' __Default value:__ `false`. (type = boolean)
#' @param scheme A string indicating a color [scheme](https://vega.github.io/vega-lite/docs/scale.html#scheme) name (e.g., `"category10"` or `"blues"`) or a [scheme parameter object](https://vega.github.io/vega-lite/docs/scale.html#scheme-params).
#' 
#' Discrete color schemes may be used with [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) or [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales. Continuous color schemes are intended for use with color scales.
#' 
#' For the full list of supported schemes, please refer to the [Vega Scheme](https://vega.github.io/vega/docs/schemes/#reference) reference. (type = Varies)
#' @param type The type of scale.  Vega-Lite supports the following categories of scale types:
#' 
#' 1) [**Continuous Scales**](https://vega.github.io/vega-lite/docs/scale.html#continuous) -- mapping continuous domains to continuous output ranges ([`"linear"`](https://vega.github.io/vega-lite/docs/scale.html#linear), [`"pow"`](https://vega.github.io/vega-lite/docs/scale.html#pow), [`"sqrt"`](https://vega.github.io/vega-lite/docs/scale.html#sqrt), [`"symlog"`](https://vega.github.io/vega-lite/docs/scale.html#symlog), [`"log"`](https://vega.github.io/vega-lite/docs/scale.html#log), [`"time"`](https://vega.github.io/vega-lite/docs/scale.html#time), [`"utc"`](https://vega.github.io/vega-lite/docs/scale.html#utc).
#' 
#' 2) [**Discrete Scales**](https://vega.github.io/vega-lite/docs/scale.html#discrete) -- mapping discrete domains to discrete ([`"ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#ordinal)) or continuous ([`"band"`](https://vega.github.io/vega-lite/docs/scale.html#band) and [`"point"`](https://vega.github.io/vega-lite/docs/scale.html#point)) output ranges.
#' 
#' 3) [**Discretizing Scales**](https://vega.github.io/vega-lite/docs/scale.html#discretizing) -- mapping continuous domains to discrete output ranges [`"bin-ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#bin-ordinal), [`"quantile"`](https://vega.github.io/vega-lite/docs/scale.html#quantile), [`"quantize"`](https://vega.github.io/vega-lite/docs/scale.html#quantize) and [`"threshold"`](https://vega.github.io/vega-lite/docs/scale.html#threshold).
#' 
#' __Default value:__ please see the [scale type table](https://vega.github.io/vega-lite/docs/scale.html#type). (type = ScaleType)
#' @param zero If `true`, ensures that a zero baseline value is included in the scale domain.
#' 
#' __Default value:__ `true` for x and y channels if the quantitative field is not binned and no custom `domain` is provided; `false` otherwise.
#' 
#' __Note:__ Log, time, and utc scales do not support `zero`. (type = boolean)
#' @return A modified spec
#' @export
vl_scale_x <- function(spec, base = NULL, bins = NULL, clamp = NULL, constant = NULL, domain = NULL, exponent = NULL, interpolate = NULL, nice = NULL, padding = NULL, paddingInner = NULL, paddingOuter = NULL, range = NULL, rangeStep = NULL, round = NULL, scheme = NULL, type = NULL, zero = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'x'))
  rlang::exec(.add_scale_to_encoding, !!!args_out)
} #' vl_scale_y
#' 
#' Add scale to y encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param base The logarithm base of the `log` scale (default `10`). (type = number)
#' @param bins An array of bin boundaries over the scale domain. If provided, axes and legends will use the bin boundaries to inform the choice of tick marks and text labels. (type = array)
#' @param clamp If `true`, values that exceed the data domain are clamped to either the minimum or maximum range value
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `clamp` (`true` by default). (type = boolean)
#' @param constant A constant determining the slope of the symlog function around zero. Only used for `symlog` scales.
#' 
#' __Default value:__ `1` (type = number)
#' @param domain Customized domain values.
#' 
#' For _quantitative_ fields, `domain` can take the form of a two-element array with minimum and maximum values.  [Piecewise scales](https://vega.github.io/vega-lite/docs/scale.html#piecewise) can be created by providing a `domain` with more than two entries.
#' If the input field is aggregated, `domain` can also be a string value `"unaggregated"`, indicating that the domain should include the raw data values prior to the aggregation.
#' 
#' For _temporal_ fields, `domain` can be a two-element array minimum and maximum values, in the form of either timestamps or the [DateTime definition objects](https://vega.github.io/vega-lite/docs/types.html#datetime).
#' 
#' For _ordinal_ and _nominal_ fields, `domain` can be an array that lists valid input values.
#' 
#' The `selection` property can be used to [interactively determine](https://vega.github.io/vega-lite/docs/selection.html#scale-domains) the scale domain. (type = Varies)
#' @param exponent The exponent of the `pow` scale. (type = number)
#' @param interpolate The interpolation method for range values. By default, a general interpolator for numbers, dates, strings and colors (in HCL space) is used. For color ranges, this property allows interpolation in alternative color spaces. Legal values include `rgb`, `hsl`, `hsl-long`, `lab`, `hcl`, `hcl-long`, `cubehelix` and `cubehelix-long` ('-long' variants use longer paths in polar coordinate spaces). If object-valued, this property accepts an object with a string-valued _type_ property and an optional numeric _gamma_ property applicable to rgb and cubehelix interpolators. For more, see the [d3-interpolate documentation](https://github.com/d3/d3-interpolate).
#' 
#' * __Default value:__ `hcl` (type = Varies)
#' @param nice Extending the domain so that it starts and ends on nice round values. This method typically modifies the scale’s domain, and may only extend the bounds to the nearest round value. Nicing is useful if the domain is computed from data and may be irregular. For example, for a domain of _[0.201479…, 0.996679…]_, a nice domain might be _[0.2, 1.0]_.
#' 
#' For quantitative scales such as linear, `nice` can be either a boolean flag or a number. If `nice` is a number, it will represent a desired tick count. This allows greater control over the step size used to extend the bounds, guaranteeing that the returned ticks will exactly cover the domain.
#' 
#' For temporal fields with time and utc scales, the `nice` value can be a string indicating the desired time interval. Legal values are `"millisecond"`, `"second"`, `"minute"`, `"hour"`, `"day"`, `"week"`, `"month"`, and `"year"`. Alternatively, `time` and `utc` scales can accept an object-valued interval specifier of the form `{"interval": "month", "step": 3}`, which includes a desired number of interval steps. Here, the domain would snap to quarter (Jan, Apr, Jul, Oct) boundaries.
#' 
#' __Default value:__ `true` for unbinned _quantitative_ fields; `false` otherwise. (type = Varies)
#' @param padding For _[continuous](https://vega.github.io/vega-lite/docs/scale.html#continuous)_ scales, expands the scale domain to accommodate the specified number of pixels on each of the scale range. The scale range must represent pixels for this parameter to function as intended. Padding adjustment is performed prior to all other adjustments, including the effects of the zero, nice, domainMin, and domainMax properties.
#' 
#' For _[band](https://vega.github.io/vega-lite/docs/scale.html#band)_ scales, shortcut for setting `paddingInner` and `paddingOuter` to the same value.
#' 
#' For _[point](https://vega.github.io/vega-lite/docs/scale.html#point)_ scales, alias for `paddingOuter`.
#' 
#' __Default value:__ For _continuous_ scales, derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `continuousPadding`.
#' For _band and point_ scales, see `paddingInner` and `paddingOuter`. (type = number)
#' @param paddingInner The inner padding (spacing) within each band step of band scales, as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' For point scale, this property is invalid as point scales do not have internal band widths (only step sizes between bands).
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingInner`. (type = number)
#' @param paddingOuter The outer padding (spacing) at the ends of the range of band and point scales,
#' as a fraction of the step size. This value must lie in the range [0,1].
#' 
#' __Default value:__ derived from the [scale config](https://vega.github.io/vega-lite/docs/scale.html#config)'s `bandPaddingOuter` for band scales and `pointPadding` for point scales. (type = number)
#' @param range The range of the scale. One of:
#' 
#' - A string indicating a [pre-defined named scale range](https://vega.github.io/vega-lite/docs/scale.html#range-config) (e.g., example, `"symbol"`, or `"diverging"`).
#' 
#' - For [continuous scales](https://vega.github.io/vega-lite/docs/scale.html#continuous), two-element array indicating  minimum and maximum values, or an array with more than two entries for specifying a [piecewise scale](https://vega.github.io/vega-lite/docs/scale.html#piecewise).
#' 
#' - For [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) and [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales, an array of desired output values.
#' 
#' __Notes:__
#' 
#' 1) For color scales you can also specify a color [`scheme`](https://vega.github.io/vega-lite/docs/scale.html#scheme) instead of `range`.
#' 
#' 2) Any directly specified `range` for `x` and `y` channels will be ignored. Range can be customized via the view's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` and `height`) or via [range steps and paddings properties](#range-step) for [band](#band) and [point](#point) scales. (type = Varies)
#' @param rangeStep The distance between the starts of adjacent bands or points in [band](https://vega.github.io/vega-lite/docs/scale.html#band) and [point](https://vega.github.io/vega-lite/docs/scale.html#point) scales.
#' 
#' If `rangeStep` is `null` or if the view contains the scale's corresponding [size](https://vega.github.io/vega-lite/docs/size.html) (`width` for `x` scales and `height` for `y` scales), `rangeStep` will be automatically determined to fit the size of the view.
#' 
#' __Default value:__  derived the [scale config](https://vega.github.io/vega-lite/docs/config.html#scale-config)'s `textXRangeStep` (`90` by default) for x-scales of `text` marks and `rangeStep` (`21` by default) for x-scales of other marks and y-scales.
#' 
#' __Warning__: If `rangeStep` is `null` and the cardinality of the scale's domain is higher than `width` or `height`, the rangeStep might become less than one pixel and the mark might not appear correctly. (type = number OR null)
#' @param round If `true`, rounds numeric output values to integers. This can be helpful for snapping to the pixel grid.
#' 
#' __Default value:__ `false`. (type = boolean)
#' @param scheme A string indicating a color [scheme](https://vega.github.io/vega-lite/docs/scale.html#scheme) name (e.g., `"category10"` or `"blues"`) or a [scheme parameter object](https://vega.github.io/vega-lite/docs/scale.html#scheme-params).
#' 
#' Discrete color schemes may be used with [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete) or [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing) scales. Continuous color schemes are intended for use with color scales.
#' 
#' For the full list of supported schemes, please refer to the [Vega Scheme](https://vega.github.io/vega/docs/schemes/#reference) reference. (type = Varies)
#' @param type The type of scale.  Vega-Lite supports the following categories of scale types:
#' 
#' 1) [**Continuous Scales**](https://vega.github.io/vega-lite/docs/scale.html#continuous) -- mapping continuous domains to continuous output ranges ([`"linear"`](https://vega.github.io/vega-lite/docs/scale.html#linear), [`"pow"`](https://vega.github.io/vega-lite/docs/scale.html#pow), [`"sqrt"`](https://vega.github.io/vega-lite/docs/scale.html#sqrt), [`"symlog"`](https://vega.github.io/vega-lite/docs/scale.html#symlog), [`"log"`](https://vega.github.io/vega-lite/docs/scale.html#log), [`"time"`](https://vega.github.io/vega-lite/docs/scale.html#time), [`"utc"`](https://vega.github.io/vega-lite/docs/scale.html#utc).
#' 
#' 2) [**Discrete Scales**](https://vega.github.io/vega-lite/docs/scale.html#discrete) -- mapping discrete domains to discrete ([`"ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#ordinal)) or continuous ([`"band"`](https://vega.github.io/vega-lite/docs/scale.html#band) and [`"point"`](https://vega.github.io/vega-lite/docs/scale.html#point)) output ranges.
#' 
#' 3) [**Discretizing Scales**](https://vega.github.io/vega-lite/docs/scale.html#discretizing) -- mapping continuous domains to discrete output ranges [`"bin-ordinal"`](https://vega.github.io/vega-lite/docs/scale.html#bin-ordinal), [`"quantile"`](https://vega.github.io/vega-lite/docs/scale.html#quantile), [`"quantize"`](https://vega.github.io/vega-lite/docs/scale.html#quantize) and [`"threshold"`](https://vega.github.io/vega-lite/docs/scale.html#threshold).
#' 
#' __Default value:__ please see the [scale type table](https://vega.github.io/vega-lite/docs/scale.html#type). (type = ScaleType)
#' @param zero If `true`, ensures that a zero baseline value is included in the scale domain.
#' 
#' __Default value:__ `true` for x and y channels if the quantitative field is not binned and no custom `domain` is provided; `false` otherwise.
#' 
#' __Note:__ Log, time, and utc scales do not support `zero`. (type = boolean)
#' @return A modified spec
#' @export
vl_scale_y <- function(spec, base = NULL, bins = NULL, clamp = NULL, constant = NULL, domain = NULL, exponent = NULL, interpolate = NULL, nice = NULL, padding = NULL, paddingInner = NULL, paddingOuter = NULL, range = NULL, rangeStep = NULL, round = NULL, scheme = NULL, type = NULL, zero = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = 'y'))
  rlang::exec(.add_scale_to_encoding, !!!args_out)
}