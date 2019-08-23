#' vl_chart
#'
#' Initialize a Vega-Lite specification! Can add any top level configuration 
#' parameters, or simply call without arguments to initialize and then use other
#' function (like [vl_mark_point()], [vl_encode_x()], etc) to add on the various
#' pieces of the chart spec. 
#' @param $schema (_TopLevelUnitSpec, TopLevelFacetSpec, TopLevelLayerSpec, TopLevelRepeatSpec, TopLevelConcatSpec, TopLevelVConcatSpec, TopLevelHConcatSpec_) URL to [JSON schema](http://json-schema.org/) for a Vega-Lite specification. Unless you have a reason to change this, use `https://vega.github.io/schema/vega-lite/v3.json`. Setting the `$schema` property allows automatic validation and autocomplete in editors that support JSON schema.
#' @param align (_TopLevelUnitSpec, TopLevelFacetSpec, TopLevelRepeatSpec, TopLevelConcatSpec_) The alignment to apply to grid rows and columns.
#' The supported string values are `"all"`, `"each"`, and `"none"`.
#' 
#' - For `"none"`, a flow layout will be used, in which adjacent subviews are simply placed one after the other.
#' - For `"each"`, subviews will be aligned into a clean grid structure, but each row or column may be of variable size.
#' - For `"all"`, subviews will be aligned and each row or column will be sized identically based on the maximum observed size. String values for this property will be applied to both grid rows and columns.
#' 
#' Alternatively, an object value of the form `{"row": string, "column": string}` can be used to supply different alignments for rows and columns.
#' 
#' __Default value:__ `"all"`.
#' @param autosize (_TopLevelUnitSpec, TopLevelFacetSpec, TopLevelLayerSpec, TopLevelRepeatSpec, TopLevelConcatSpec, TopLevelVConcatSpec, TopLevelHConcatSpec_) Sets how the visualization size should be determined. If a string, should be one of `"pad"`, `"fit"` or `"none"`.
#' Object values can additionally specify parameters for content sizing and automatic resizing.
#' `"fit"` is only supported for single and layered views that don't use `rangeStep`.
#' 
#' __Default value__: `pad`
#' @param background (_TopLevelUnitSpec, TopLevelFacetSpec, TopLevelLayerSpec, TopLevelRepeatSpec, TopLevelConcatSpec, TopLevelVConcatSpec, TopLevelHConcatSpec_) CSS color property to use as the background of the entire view.
#' 
#' __Default value:__ none (transparent)
#' @param bounds (_TopLevelUnitSpec, TopLevelFacetSpec, TopLevelRepeatSpec, TopLevelConcatSpec, TopLevelVConcatSpec, TopLevelHConcatSpec_) The bounds calculation method to use for determining the extent of a sub-plot. One of `full` (the default) or `flush`.
#' 
#' - If set to `full`, the entire calculated bounds (including axes, title, and legend) will be used.
#' - If set to `flush`, only the specified width and height values for the sub-view will be used. The `flush` setting can be useful when attempting to place sub-plots without axes or legends into a uniform grid structure.
#' 
#' __Default value:__ `"full"`
#' @param center (_TopLevelVConcatSpec, TopLevelHConcatSpec_) Boolean flag indicating if subviews should be centered relative to their respective rows or columns.
#' 
#' __Default value:__ `false`
#' 
#' (_TopLevelUnitSpec, TopLevelFacetSpec, TopLevelRepeatSpec, TopLevelConcatSpec_) Boolean flag indicating if subviews should be centered relative to their respective rows or columns.
#' 
#' An object value of the form `{"row": boolean, "column": boolean}` can be used to supply different centering values for rows and columns.
#' 
#' __Default value:__ `false`
#' @param columns (_TopLevelUnitSpec, TopLevelFacetSpec, TopLevelRepeatSpec, TopLevelConcatSpec_) The number of columns to include in the view composition layout.
#' 
#' __Default value__: `undefined` -- An infinite number of columns (a single row) will be assumed. This is equivalent to
#' `hconcat` (for `concat`) and to using the `column` channel (for `facet` and `repeat`).
#' 
#' __Note__:
#' 
#' 1) This property is only for:
#' - the general (wrappable) `concat` operator (not `hconcat`/`vconcat`)
#' - the `facet` and `repeat` operator with one field/repetition definition (without row/column nesting)
#' 
#' 2) Setting the `columns` to `1` is equivalent to `vconcat` (for `concat`) and to using the `row` channel (for `facet` and `repeat`).
#' @param config (_TopLevelUnitSpec, TopLevelFacetSpec, TopLevelLayerSpec, TopLevelRepeatSpec, TopLevelConcatSpec, TopLevelVConcatSpec, TopLevelHConcatSpec_) Vega-Lite configuration object.  This property can only be defined at the top-level of a specification.
#' @param data (_TopLevelUnitSpec, TopLevelFacetSpec, TopLevelLayerSpec, TopLevelRepeatSpec, TopLevelConcatSpec, TopLevelVConcatSpec, TopLevelHConcatSpec_) An object describing the data source
#' @param datasets (_TopLevelUnitSpec, TopLevelFacetSpec, TopLevelLayerSpec, TopLevelRepeatSpec, TopLevelConcatSpec, TopLevelVConcatSpec, TopLevelHConcatSpec_) A global data store for named datasets. This is a mapping from names to inline datasets.
#' This can be an array of objects or primitive values or a string. Arrays of primitive values are ingested as objects with a `data` property.
#' @param description (_TopLevelUnitSpec, TopLevelFacetSpec, TopLevelLayerSpec, TopLevelRepeatSpec, TopLevelConcatSpec, TopLevelVConcatSpec, TopLevelHConcatSpec_) Description of this mark for commenting purpose.
#' @param encoding (_TopLevelUnitSpec_) A key-value mapping between encoding channels and definition of fields.
#' 
#' (_TopLevelLayerSpec_) A shared key-value mapping between encoding channels and definition of fields in the underlying layers.
#' @param height (_TopLevelUnitSpec, TopLevelLayerSpec_) The height of a visualization.
#' 
#' __Default value:__
#' - If a view's [`autosize`](https://vega.github.io/vega-lite/docs/size.html#autosize) type is `"fit"` or its y-channel has a [continuous scale](https://vega.github.io/vega-lite/docs/scale.html#continuous), the height will be the value of [`config.view.height`](https://vega.github.io/vega-lite/docs/spec.html#config).
#' - For y-axis with a band or point scale: if [`rangeStep`](https://vega.github.io/vega-lite/docs/scale.html#band) is a numeric value or unspecified, the height is [determined by the range step, paddings, and the cardinality of the field mapped to y-channel](https://vega.github.io/vega-lite/docs/scale.html#band). Otherwise, if the `rangeStep` is `null`, the height will be the value of [`config.view.height`](https://vega.github.io/vega-lite/docs/spec.html#config).
#' - If no field is mapped to `y` channel, the `height` will be the value of `rangeStep`.
#' 
#' __Note__: For plots with [`row` and `column` channels](https://vega.github.io/vega-lite/docs/encoding.html#facet), this represents the height of a single view.
#' 
#' __See also:__ The documentation for [width and height](https://vega.github.io/vega-lite/docs/size.html) contains more examples.
#' @param mark (_TopLevelUnitSpec_) A string describing the mark type (one of `"bar"`, `"circle"`, `"square"`, `"tick"`, `"line"`,
#' `"area"`, `"point"`, `"rule"`, `"geoshape"`, and `"text"`) or a [mark definition object](https://vega.github.io/vega-lite/docs/mark.html#mark-def).
#' @param name (_TopLevelUnitSpec, TopLevelFacetSpec, TopLevelLayerSpec, TopLevelRepeatSpec, TopLevelConcatSpec, TopLevelVConcatSpec, TopLevelHConcatSpec_) Name of the visualization for later reference.
#' @param padding (_TopLevelUnitSpec, TopLevelFacetSpec, TopLevelLayerSpec, TopLevelRepeatSpec, TopLevelConcatSpec, TopLevelVConcatSpec, TopLevelHConcatSpec_) The default visualization padding, in pixels, from the edge of the visualization canvas to the data rectangle.  If a number, specifies padding for all sides.
#' If an object, the value should have the format `{"left": 5, "top": 5, "right": 5, "bottom": 5}` to specify padding for each side of the visualization.
#' 
#' __Default value__: `5`
#' @param projection (_TopLevelUnitSpec_) An object defining properties of geographic projection, which will be applied to `shape` path for `"geoshape"` marks
#' and to `latitude` and `"longitude"` channels for other marks.
#' 
#' (_TopLevelLayerSpec_) An object defining properties of the geographic projection shared by underlying layers.
#' @param resolve (_TopLevelUnitSpec, TopLevelFacetSpec, TopLevelLayerSpec, TopLevelRepeatSpec, TopLevelConcatSpec, TopLevelVConcatSpec, TopLevelHConcatSpec_) Scale, axis, and legend resolutions for view composition specifications.
#' @param selection (_TopLevelUnitSpec_) A key-value mapping between selection names and definitions.
#' @param spacing (_TopLevelUnitSpec, TopLevelFacetSpec, TopLevelRepeatSpec, TopLevelConcatSpec_) The spacing in pixels between sub-views of the composition operator.
#' An object of the form `{"row": number, "column": number}` can be used to set
#' different spacing values for rows and columns.
#' 
#' __Default value__: Depends on `"spacing"` property of [the view composition configuration](https://vega.github.io/vega-lite/docs/config.html#view-config) (`20` by default)
#' 
#' (_TopLevelVConcatSpec, TopLevelHConcatSpec_) The spacing in pixels between sub-views of the concat operator.
#' 
#' __Default value__: `10`
#' @param title (_TopLevelUnitSpec, TopLevelFacetSpec, TopLevelLayerSpec, TopLevelRepeatSpec, TopLevelConcatSpec, TopLevelVConcatSpec, TopLevelHConcatSpec_) Title for the plot.
#' @param transform (_TopLevelUnitSpec, TopLevelFacetSpec, TopLevelLayerSpec, TopLevelRepeatSpec, TopLevelConcatSpec, TopLevelVConcatSpec, TopLevelHConcatSpec_) An array of data transformations such as filter and new field calculation.
#' @param usermeta (_TopLevelUnitSpec, TopLevelFacetSpec, TopLevelLayerSpec, TopLevelRepeatSpec, TopLevelConcatSpec, TopLevelVConcatSpec, TopLevelHConcatSpec_) Optional metadata that will be passed to Vega.
#' This object is completely ignored by Vega and Vega-Lite and can be used for custom metadata.
#' @param view (_TopLevelUnitSpec, TopLevelLayerSpec_) An object defining the view background's fill and stroke.
#' 
#' __Default value:__ none (transparent)
#' @param width (_TopLevelUnitSpec, TopLevelLayerSpec_) The width of a visualization.
#' 
#' __Default value:__ This will be determined by the following rules:
#' 
#' - If a view's [`autosize`](https://vega.github.io/vega-lite/docs/size.html#autosize) type is `"fit"` or its x-channel has a [continuous scale](https://vega.github.io/vega-lite/docs/scale.html#continuous), the width will be the value of [`config.view.width`](https://vega.github.io/vega-lite/docs/spec.html#config).
#' - For x-axis with a band or point scale: if [`rangeStep`](https://vega.github.io/vega-lite/docs/scale.html#band) is a numeric value or unspecified, the width is [determined by the range step, paddings, and the cardinality of the field mapped to x-channel](https://vega.github.io/vega-lite/docs/scale.html#band).   Otherwise, if the `rangeStep` is `null`, the width will be the value of [`config.view.width`](https://vega.github.io/vega-lite/docs/spec.html#config).
#' - If no field is mapped to `x` channel, the `width` will be the value of [`config.scale.textXRangeStep`](https://vega.github.io/vega-lite/docs/size.html#default-width-and-height) for `text` mark and the value of `rangeStep` for other marks.
#' 
#' __Note:__ For plots with [`row` and `column` channels](https://vega.github.io/vega-lite/docs/encoding.html#facet), this represents the width of a single view.
#' 
#' __See also:__ The documentation for [width and height](https://vega.github.io/vega-lite/docs/size.html) contains more examples.
#' @param facet (_TopLevelFacetSpec_) Definition for how to facet the data.  One of:
#' 1) [a field definition for faceting the plot by one field](https://vega.github.io/vega-lite/docs/facet.html#field-def)
#' 2) [An object that maps `row` and `column` channels to their field definitions](https://vega.github.io/vega-lite/docs/facet.html#mapping)
#' @param spec (_TopLevelFacetSpec_) A specification of the view that gets faceted.
#' 
#' (_TopLevelRepeatSpec_) A specification of the view that gets repeated.
#' @param layer (_TopLevelLayerSpec_) Layer or single view specifications to be layered.
#' 
#' __Note__: Specifications inside `layer` cannot use `row` and `column` channels as layering facet specifications is not allowed. Instead, use the [facet operator](https://vega.github.io/vega-lite/docs/facet.html) and place a layer inside a facet.
#' @param repeat (_TopLevelRepeatSpec_) Definition for fields to be repeated.  One of:
#' 1) An array of fields to be repeated.  If `"repeat"` is an array, the field can be referred using `{"repeat": "repeat"}`
#' 2) An object that mapped `"row"` and/or `"column"` to the listed of fields to be repeated along the particular orientations. The objects `{"repeat": "row"}` and `{"repeat": "column"}` can be used to refer to the repeated field respectively.
#' @param concat (_TopLevelConcatSpec_) A list of views to be concatenated.
#' @param vconcat (_TopLevelVConcatSpec_) A list of views to be concatenated and put into a column.
#' @param hconcat (_TopLevelHConcatSpec_) A list of views to be concatenated and put into a row.
#'
#' @return A vega-lite spec, as an S3 object of class vegaspec_vega_lite
#'  using [vegawidget::as_vegaspec()]
#' @export
#' @importFrom utils hasName
#' @examples 
#' 
#' vl_chart() %>%
#'   vl_add_data(values = mtcars) %>%
#'   vl_mark_point() %>%
#'   vl_encode_x("wt") %>%
#'   vl_encode_y("mpg") 
vl_chart <- function(data = NULL, `$schema` = vegawidget::vega_schema(), align = NULL, autosize = NULL, background = NULL, bounds = NULL, center = NULL, columns = NULL, config = NULL, datasets = NULL, description = NULL, encoding = NULL, height = NULL, mark = NULL, name = NULL, padding = NULL, projection = NULL, resolve = NULL, selection = NULL, spacing = NULL, title = NULL, transform = NULL, usermeta = NULL, view = NULL, width = NULL, facet = NULL, spec = NULL, layer = NULL, `repeat` = NULL, concat = NULL, vconcat = NULL, hconcat = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  vegawidget::as_vegaspec(args_out)
}
 #' vl_add_data
#'
#' Add data to a vega-lite spec
#' @param spec An input vega-lite spec
#' @param format (_UrlData, InlineData, NamedData_) An object that specifies the format for parsing the data.
#' @param name (_UrlData, InlineData, NamedData, SequenceGenerator, SphereGenerator, GraticuleGenerator_) Provide a placeholder name and bind data at runtime.
#' @param url (_UrlData_) An URL from which to load the data set. Use the `format.type` property
#' to ensure the loaded data is correctly parsed.
#' @param values (_InlineData_) The full data set, included inline. This can be an array of objects or primitive values, an object, or a string.
#' Arrays of primitive values are ingested as objects with a `data` property. Strings are parsed according to the specified format type.
#' @param sequence (_SequenceGenerator_) Generate a sequence of numbers.
#' @param sphere (_SphereGenerator_) Generate sphere GeoJSON data for the full globe.
#' @param graticule (_GraticuleGenerator_) Generate graticule GeoJSON data for geographic reference lines.
#' @return A modified Vega-Lite Spec
#' @export
vl_add_data <- function(spec, format = NULL, name = NULL, url = NULL, values = NULL, sequence = NULL, sphere = NULL, graticule = NULL){
  args <- .modify_args(NULL, c("format", "name", "url", "values", "sequence", "sphere", "graticule"))
  .add_data(args$spec, args$object, '#/definitions/Data', args$extra)
} #' vl_mark_area
#'
#'
#' @param spec An input vega-lite spec
#' @param align (_MarkDef_) The horizontal alignment of the text. One of `"left"`, `"right"`, `"center"`.
#' @param angle (_MarkDef_) The rotation angle of the text, in degrees.
#' @param baseline (_MarkDef_) The vertical alignment of the text. One of `"top"`, `"middle"`, `"bottom"`.
#' 
#' __Default value:__ `"middle"`
#' @param binSpacing (_MarkDef_) Offset between bars for binned field.  Ideal value for this is either 0 (Preferred by statisticians) or 1 (Vega-Lite Default, D3 example style).
#' 
#' __Default value:__ `1`
#' @param clip (_MarkDef_) Whether a mark be clipped to the enclosing group’s width and height.
#' @param color (_MarkDef_) Default color.  Note that `fill` and `stroke` have higher precedence than `color` and will override `color`.
#' 
#' __Default value:__ <span style="color: #4682b4;">&#9632;</span> `"#4682b4"`
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param cornerRadius (_MarkDef_) The radius in pixels of rounded rectangle corners.
#' 
#' __Default value:__ `0`
#' @param cursor (_MarkDef_) The mouse cursor used over the mark. Any valid [CSS cursor type](https://developer.mozilla.org/en-US/docs/Web/CSS/cursor#Values) can be used.
#' @param dir (_MarkDef_) The direction of the text. One of `"ltr"` (left-to-right) or `"rtl"` (right-to-left). This property determines on which side is truncated in response to the limit parameter.
#' 
#' __Default value:__ `"ltr"`
#' @param dx (_MarkDef_) The horizontal offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param dy (_MarkDef_) The vertical offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param ellipsis (_MarkDef_) The ellipsis string for text truncated in response to the limit parameter.
#' 
#' __Default value:__ `"…"`
#' @param fill (_MarkDef_) Default Fill Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param fillOpacity (_MarkDef_) The fill opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param filled (_MarkDef_) Whether the mark's color should be used as fill color instead of stroke color.
#' 
#' __Default value:__ `false` for `point`, `line` and `rule`; otherwise, `true`.
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param font (_MarkDef_) The typeface to set the text in (e.g., `"Helvetica Neue"`).
#' @param fontSize (_MarkDef_) The font size, in pixels.
#' @param fontStyle (_MarkDef_) The font style (e.g., `"italic"`).
#' @param fontWeight (_MarkDef_) The font weight.
#' This can be either a string (e.g `"bold"`, `"normal"`) or a number (`100`, `200`, `300`, ..., `900` where `"normal"` = `400` and `"bold"` = `700`).
#' @param href (_MarkDef_) A URL to load upon mouse click. If defined, the mark acts as a hyperlink.
#' @param interpolate (_MarkDef_) The line interpolation method to use for line and area marks. One of the following:
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
#' - `"monotone"`: cubic interpolation that preserves monotonicity in y.
#' @param limit (_MarkDef_) The maximum length of the text mark in pixels. The text value will be automatically truncated if the rendered size exceeds the limit.
#' 
#' __Default value:__ `0`, indicating no limit
#' @param line (_MarkDef_) A flag for overlaying line on top of area marks, or an object defining the properties of the overlayed lines.
#' 
#' - If this value is an empty object (`{}`) or `true`, lines with default properties will be used.
#' 
#' - If this value is `false`, no lines would be automatically added to area marks.
#' 
#' __Default value:__ `false`.
#' @param opacity (_MarkDef_) The overall opacity (value between \[0,1\]).
#' 
#' __Default value:__ `0.7` for non-aggregate plots with `point`, `tick`, `circle`, or `square` marks or layered `bar` charts and `1` otherwise.
#' @param order (_MarkDef_) For line and trail marks, this `order` property can be set to `null` or `false` to make the lines use the original order in the data sources.
#' @param orient (_MarkDef_) The orientation of a non-stacked bar, tick, area, and line charts.
#' The value is either horizontal (default) or vertical.
#' - For bar, rule and tick, this determines whether the size of the bar and tick
#' should be applied to x or y dimension.
#' - For area, this property determines the orient property of the Vega output.
#' - For line and trail marks, this property determines the sort order of the points in the line
#' if `config.sortLineBy` is not specified.
#' For stacked charts, this is always determined by the orientation of the stack;
#' therefore explicitly specified value will be ignored.
#' @param point (_MarkDef_) A flag for overlaying points on top of line or area marks, or an object defining the properties of the overlayed points.
#' 
#' - If this property is `"transparent"`, transparent points will be used (for enhancing tooltips and selections).
#' 
#' - If this property is an empty object (`{}`) or `true`, filled points with default properties will be used.
#' 
#' - If this property is `false`, no points would be automatically added to line or area marks.
#' 
#' __Default value:__ `false`.
#' @param radius (_MarkDef_) Polar coordinate radial offset, in pixels, of the text label from the origin determined by the `x` and `y` properties.
#' @param shape (_MarkDef_) Shape of the point marks. Supported values include:
#' - plotting shapes: `"circle"`, `"square"`, `"cross"`, `"diamond"`, `"triangle-up"`, `"triangle-down"`, `"triangle-right"`, or `"triangle-left"`.
#' - the line symbol `"stroke"`
#' - centered directional shapes `"arrow"`, `"wedge"`, or `"triangle"`
#' - a custom [SVG path string](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) (For correct sizing, custom shape paths should be defined within a square bounding box with coordinates ranging from -1 to 1 along both the x and y dimensions.)
#' 
#' __Default value:__ `"circle"`
#' @param size (_MarkDef_) Default size for marks.
#' - For `point`/`circle`/`square`, this represents the pixel area of the marks. For example: in the case of circles, the radius is determined in part by the square root of the size value.
#' - For `bar`, this represents the band size of the bar, in pixels.
#' - For `text`, this represents the font size, in pixels.
#' 
#' __Default value:__ `30` for point, circle, square marks; `rangeStep` - 1 for bar marks with discrete dimensions; `5` for bar marks with continuous dimensions; `11` for text marks.
#' @param stroke (_MarkDef_) Default Stroke Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param strokeCap (_MarkDef_) The stroke cap for line ending style. One of `"butt"`, `"round"`, or `"square"`.
#' 
#' __Default value:__ `"square"`
#' @param strokeDash (_MarkDef_) An array of alternating stroke, space lengths for creating dashed or dotted lines.
#' @param strokeDashOffset (_MarkDef_) The offset (in pixels) into which to begin drawing with the stroke dash array.
#' @param strokeJoin (_MarkDef_) The stroke line join method. One of `"miter"`, `"round"` or `"bevel"`.
#' 
#' __Default value:__ `"miter"`
#' @param strokeMiterLimit (_MarkDef_) The miter limit at which to bevel a line join.
#' @param strokeOpacity (_MarkDef_) The stroke opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param strokeWidth (_MarkDef_) The stroke width, in pixels.
#' @param style (_MarkDef_) A string or array of strings indicating the name of custom styles to apply to the mark. A style is a named collection of mark property defaults defined within the [style configuration](https://vega.github.io/vega-lite/docs/mark.html#style-config). If style is an array, later styles will override earlier styles. Any [mark properties](https://vega.github.io/vega-lite/docs/encoding.html#mark-prop) explicitly defined within the `encoding` will override a style default.
#' 
#' __Default value:__ The mark's name.  For example, a bar mark will have style `"bar"` by default.
#' __Note:__ Any specified style will augment the default style. For example, a bar mark with `"style": "foo"` will receive from `config.style.bar` and `config.style.foo` (the specified style `"foo"` has higher precedence).
#' @param tension (_MarkDef_) Depending on the interpolation type, sets the tension parameter (for line and area marks).
#' @param text (_MarkDef_) Placeholder text if the `text` channel is not specified
#' @param theta (_MarkDef_) Polar coordinate angle, in radians, of the text label from the origin determined by the `x` and `y` properties. Values for `theta` follow the same convention of `arc` mark `startAngle` and `endAngle` properties: angles are measured in radians, with `0` indicating "north".
#' @param thickness (_MarkDef_) Thickness of the tick mark.
#' 
#' __Default value:__  `1`
#' @param tooltip (_MarkDef_) The tooltip text string to show upon mouse hover or an object defining which fields should the tooltip be derived from.
#' 
#' - If `tooltip` is `{"content": "encoding"}`, then all fields from `encoding` will be used.
#' - If `tooltip` is `{"content": "data"}`, then all fields that appear in the highlighted data point will be used.
#' - If set to `null`, then no tooltip will be used.
#' @param x (_MarkDef_) X coordinates of the marks, or width of horizontal `"bar"` and `"area"` without `x2`.
#' @param x2 (_MarkDef_) X2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param x2Offset (_MarkDef_) Offset for x2-position.
#' @param xOffset (_MarkDef_) Offset for x-position.
#' @param y (_MarkDef_) Y coordinates of the marks, or height of vertical `"bar"` and `"area"` without `y2`
#' @param y2 (_MarkDef_) Y2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param y2Offset (_MarkDef_) Offset for y2-position.
#' @param yOffset (_MarkDef_) Offset for y-position.
#' @return A modified Vega-Lite Spec
#' @export
vl_mark_area <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL){
  args <- .modify_args(list(type = "area"), c("align", "angle", "baseline", "binSpacing", "clip", "color", "cornerRadius", 
  "cursor", "dir", "dx", "dy", "ellipsis", "fill", "fillOpacity", "filled", "font", 
  "fontSize", "fontStyle", "fontWeight", "href", "interpolate", "limit", "line", 
  "opacity", "order", "orient", "point", "radius", "shape", "size", "stroke", 
  "strokeCap", "strokeDash", "strokeDashOffset", "strokeJoin", "strokeMiterLimit", 
  "strokeOpacity", "strokeWidth", "style", "tension", "text", "theta", "thickness", 
  "tooltip", "type", "x", "x2", "x2Offset", "xOffset", "y", "y2", "y2Offset", 
  "yOffset"))
  .add_mark(args$spec, args$object, '#/definitions/MarkDef', args$extra)
} #' vl_mark_bar
#'
#'
#' @param spec An input vega-lite spec
#' @param align (_MarkDef_) The horizontal alignment of the text. One of `"left"`, `"right"`, `"center"`.
#' @param angle (_MarkDef_) The rotation angle of the text, in degrees.
#' @param baseline (_MarkDef_) The vertical alignment of the text. One of `"top"`, `"middle"`, `"bottom"`.
#' 
#' __Default value:__ `"middle"`
#' @param binSpacing (_MarkDef_) Offset between bars for binned field.  Ideal value for this is either 0 (Preferred by statisticians) or 1 (Vega-Lite Default, D3 example style).
#' 
#' __Default value:__ `1`
#' @param clip (_MarkDef_) Whether a mark be clipped to the enclosing group’s width and height.
#' @param color (_MarkDef_) Default color.  Note that `fill` and `stroke` have higher precedence than `color` and will override `color`.
#' 
#' __Default value:__ <span style="color: #4682b4;">&#9632;</span> `"#4682b4"`
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param cornerRadius (_MarkDef_) The radius in pixels of rounded rectangle corners.
#' 
#' __Default value:__ `0`
#' @param cursor (_MarkDef_) The mouse cursor used over the mark. Any valid [CSS cursor type](https://developer.mozilla.org/en-US/docs/Web/CSS/cursor#Values) can be used.
#' @param dir (_MarkDef_) The direction of the text. One of `"ltr"` (left-to-right) or `"rtl"` (right-to-left). This property determines on which side is truncated in response to the limit parameter.
#' 
#' __Default value:__ `"ltr"`
#' @param dx (_MarkDef_) The horizontal offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param dy (_MarkDef_) The vertical offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param ellipsis (_MarkDef_) The ellipsis string for text truncated in response to the limit parameter.
#' 
#' __Default value:__ `"…"`
#' @param fill (_MarkDef_) Default Fill Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param fillOpacity (_MarkDef_) The fill opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param filled (_MarkDef_) Whether the mark's color should be used as fill color instead of stroke color.
#' 
#' __Default value:__ `false` for `point`, `line` and `rule`; otherwise, `true`.
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param font (_MarkDef_) The typeface to set the text in (e.g., `"Helvetica Neue"`).
#' @param fontSize (_MarkDef_) The font size, in pixels.
#' @param fontStyle (_MarkDef_) The font style (e.g., `"italic"`).
#' @param fontWeight (_MarkDef_) The font weight.
#' This can be either a string (e.g `"bold"`, `"normal"`) or a number (`100`, `200`, `300`, ..., `900` where `"normal"` = `400` and `"bold"` = `700`).
#' @param href (_MarkDef_) A URL to load upon mouse click. If defined, the mark acts as a hyperlink.
#' @param interpolate (_MarkDef_) The line interpolation method to use for line and area marks. One of the following:
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
#' - `"monotone"`: cubic interpolation that preserves monotonicity in y.
#' @param limit (_MarkDef_) The maximum length of the text mark in pixels. The text value will be automatically truncated if the rendered size exceeds the limit.
#' 
#' __Default value:__ `0`, indicating no limit
#' @param line (_MarkDef_) A flag for overlaying line on top of area marks, or an object defining the properties of the overlayed lines.
#' 
#' - If this value is an empty object (`{}`) or `true`, lines with default properties will be used.
#' 
#' - If this value is `false`, no lines would be automatically added to area marks.
#' 
#' __Default value:__ `false`.
#' @param opacity (_MarkDef_) The overall opacity (value between \[0,1\]).
#' 
#' __Default value:__ `0.7` for non-aggregate plots with `point`, `tick`, `circle`, or `square` marks or layered `bar` charts and `1` otherwise.
#' @param order (_MarkDef_) For line and trail marks, this `order` property can be set to `null` or `false` to make the lines use the original order in the data sources.
#' @param orient (_MarkDef_) The orientation of a non-stacked bar, tick, area, and line charts.
#' The value is either horizontal (default) or vertical.
#' - For bar, rule and tick, this determines whether the size of the bar and tick
#' should be applied to x or y dimension.
#' - For area, this property determines the orient property of the Vega output.
#' - For line and trail marks, this property determines the sort order of the points in the line
#' if `config.sortLineBy` is not specified.
#' For stacked charts, this is always determined by the orientation of the stack;
#' therefore explicitly specified value will be ignored.
#' @param point (_MarkDef_) A flag for overlaying points on top of line or area marks, or an object defining the properties of the overlayed points.
#' 
#' - If this property is `"transparent"`, transparent points will be used (for enhancing tooltips and selections).
#' 
#' - If this property is an empty object (`{}`) or `true`, filled points with default properties will be used.
#' 
#' - If this property is `false`, no points would be automatically added to line or area marks.
#' 
#' __Default value:__ `false`.
#' @param radius (_MarkDef_) Polar coordinate radial offset, in pixels, of the text label from the origin determined by the `x` and `y` properties.
#' @param shape (_MarkDef_) Shape of the point marks. Supported values include:
#' - plotting shapes: `"circle"`, `"square"`, `"cross"`, `"diamond"`, `"triangle-up"`, `"triangle-down"`, `"triangle-right"`, or `"triangle-left"`.
#' - the line symbol `"stroke"`
#' - centered directional shapes `"arrow"`, `"wedge"`, or `"triangle"`
#' - a custom [SVG path string](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) (For correct sizing, custom shape paths should be defined within a square bounding box with coordinates ranging from -1 to 1 along both the x and y dimensions.)
#' 
#' __Default value:__ `"circle"`
#' @param size (_MarkDef_) Default size for marks.
#' - For `point`/`circle`/`square`, this represents the pixel area of the marks. For example: in the case of circles, the radius is determined in part by the square root of the size value.
#' - For `bar`, this represents the band size of the bar, in pixels.
#' - For `text`, this represents the font size, in pixels.
#' 
#' __Default value:__ `30` for point, circle, square marks; `rangeStep` - 1 for bar marks with discrete dimensions; `5` for bar marks with continuous dimensions; `11` for text marks.
#' @param stroke (_MarkDef_) Default Stroke Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param strokeCap (_MarkDef_) The stroke cap for line ending style. One of `"butt"`, `"round"`, or `"square"`.
#' 
#' __Default value:__ `"square"`
#' @param strokeDash (_MarkDef_) An array of alternating stroke, space lengths for creating dashed or dotted lines.
#' @param strokeDashOffset (_MarkDef_) The offset (in pixels) into which to begin drawing with the stroke dash array.
#' @param strokeJoin (_MarkDef_) The stroke line join method. One of `"miter"`, `"round"` or `"bevel"`.
#' 
#' __Default value:__ `"miter"`
#' @param strokeMiterLimit (_MarkDef_) The miter limit at which to bevel a line join.
#' @param strokeOpacity (_MarkDef_) The stroke opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param strokeWidth (_MarkDef_) The stroke width, in pixels.
#' @param style (_MarkDef_) A string or array of strings indicating the name of custom styles to apply to the mark. A style is a named collection of mark property defaults defined within the [style configuration](https://vega.github.io/vega-lite/docs/mark.html#style-config). If style is an array, later styles will override earlier styles. Any [mark properties](https://vega.github.io/vega-lite/docs/encoding.html#mark-prop) explicitly defined within the `encoding` will override a style default.
#' 
#' __Default value:__ The mark's name.  For example, a bar mark will have style `"bar"` by default.
#' __Note:__ Any specified style will augment the default style. For example, a bar mark with `"style": "foo"` will receive from `config.style.bar` and `config.style.foo` (the specified style `"foo"` has higher precedence).
#' @param tension (_MarkDef_) Depending on the interpolation type, sets the tension parameter (for line and area marks).
#' @param text (_MarkDef_) Placeholder text if the `text` channel is not specified
#' @param theta (_MarkDef_) Polar coordinate angle, in radians, of the text label from the origin determined by the `x` and `y` properties. Values for `theta` follow the same convention of `arc` mark `startAngle` and `endAngle` properties: angles are measured in radians, with `0` indicating "north".
#' @param thickness (_MarkDef_) Thickness of the tick mark.
#' 
#' __Default value:__  `1`
#' @param tooltip (_MarkDef_) The tooltip text string to show upon mouse hover or an object defining which fields should the tooltip be derived from.
#' 
#' - If `tooltip` is `{"content": "encoding"}`, then all fields from `encoding` will be used.
#' - If `tooltip` is `{"content": "data"}`, then all fields that appear in the highlighted data point will be used.
#' - If set to `null`, then no tooltip will be used.
#' @param x (_MarkDef_) X coordinates of the marks, or width of horizontal `"bar"` and `"area"` without `x2`.
#' @param x2 (_MarkDef_) X2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param x2Offset (_MarkDef_) Offset for x2-position.
#' @param xOffset (_MarkDef_) Offset for x-position.
#' @param y (_MarkDef_) Y coordinates of the marks, or height of vertical `"bar"` and `"area"` without `y2`
#' @param y2 (_MarkDef_) Y2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param y2Offset (_MarkDef_) Offset for y2-position.
#' @param yOffset (_MarkDef_) Offset for y-position.
#' @return A modified Vega-Lite Spec
#' @export
vl_mark_bar <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL){
  args <- .modify_args(list(type = "bar"), c("align", "angle", "baseline", "binSpacing", "clip", "color", "cornerRadius", 
  "cursor", "dir", "dx", "dy", "ellipsis", "fill", "fillOpacity", "filled", "font", 
  "fontSize", "fontStyle", "fontWeight", "href", "interpolate", "limit", "line", 
  "opacity", "order", "orient", "point", "radius", "shape", "size", "stroke", 
  "strokeCap", "strokeDash", "strokeDashOffset", "strokeJoin", "strokeMiterLimit", 
  "strokeOpacity", "strokeWidth", "style", "tension", "text", "theta", "thickness", 
  "tooltip", "type", "x", "x2", "x2Offset", "xOffset", "y", "y2", "y2Offset", 
  "yOffset"))
  .add_mark(args$spec, args$object, '#/definitions/MarkDef', args$extra)
} #' vl_mark_line
#'
#'
#' @param spec An input vega-lite spec
#' @param align (_MarkDef_) The horizontal alignment of the text. One of `"left"`, `"right"`, `"center"`.
#' @param angle (_MarkDef_) The rotation angle of the text, in degrees.
#' @param baseline (_MarkDef_) The vertical alignment of the text. One of `"top"`, `"middle"`, `"bottom"`.
#' 
#' __Default value:__ `"middle"`
#' @param binSpacing (_MarkDef_) Offset between bars for binned field.  Ideal value for this is either 0 (Preferred by statisticians) or 1 (Vega-Lite Default, D3 example style).
#' 
#' __Default value:__ `1`
#' @param clip (_MarkDef_) Whether a mark be clipped to the enclosing group’s width and height.
#' @param color (_MarkDef_) Default color.  Note that `fill` and `stroke` have higher precedence than `color` and will override `color`.
#' 
#' __Default value:__ <span style="color: #4682b4;">&#9632;</span> `"#4682b4"`
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param cornerRadius (_MarkDef_) The radius in pixels of rounded rectangle corners.
#' 
#' __Default value:__ `0`
#' @param cursor (_MarkDef_) The mouse cursor used over the mark. Any valid [CSS cursor type](https://developer.mozilla.org/en-US/docs/Web/CSS/cursor#Values) can be used.
#' @param dir (_MarkDef_) The direction of the text. One of `"ltr"` (left-to-right) or `"rtl"` (right-to-left). This property determines on which side is truncated in response to the limit parameter.
#' 
#' __Default value:__ `"ltr"`
#' @param dx (_MarkDef_) The horizontal offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param dy (_MarkDef_) The vertical offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param ellipsis (_MarkDef_) The ellipsis string for text truncated in response to the limit parameter.
#' 
#' __Default value:__ `"…"`
#' @param fill (_MarkDef_) Default Fill Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param fillOpacity (_MarkDef_) The fill opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param filled (_MarkDef_) Whether the mark's color should be used as fill color instead of stroke color.
#' 
#' __Default value:__ `false` for `point`, `line` and `rule`; otherwise, `true`.
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param font (_MarkDef_) The typeface to set the text in (e.g., `"Helvetica Neue"`).
#' @param fontSize (_MarkDef_) The font size, in pixels.
#' @param fontStyle (_MarkDef_) The font style (e.g., `"italic"`).
#' @param fontWeight (_MarkDef_) The font weight.
#' This can be either a string (e.g `"bold"`, `"normal"`) or a number (`100`, `200`, `300`, ..., `900` where `"normal"` = `400` and `"bold"` = `700`).
#' @param href (_MarkDef_) A URL to load upon mouse click. If defined, the mark acts as a hyperlink.
#' @param interpolate (_MarkDef_) The line interpolation method to use for line and area marks. One of the following:
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
#' - `"monotone"`: cubic interpolation that preserves monotonicity in y.
#' @param limit (_MarkDef_) The maximum length of the text mark in pixels. The text value will be automatically truncated if the rendered size exceeds the limit.
#' 
#' __Default value:__ `0`, indicating no limit
#' @param line (_MarkDef_) A flag for overlaying line on top of area marks, or an object defining the properties of the overlayed lines.
#' 
#' - If this value is an empty object (`{}`) or `true`, lines with default properties will be used.
#' 
#' - If this value is `false`, no lines would be automatically added to area marks.
#' 
#' __Default value:__ `false`.
#' @param opacity (_MarkDef_) The overall opacity (value between \[0,1\]).
#' 
#' __Default value:__ `0.7` for non-aggregate plots with `point`, `tick`, `circle`, or `square` marks or layered `bar` charts and `1` otherwise.
#' @param order (_MarkDef_) For line and trail marks, this `order` property can be set to `null` or `false` to make the lines use the original order in the data sources.
#' @param orient (_MarkDef_) The orientation of a non-stacked bar, tick, area, and line charts.
#' The value is either horizontal (default) or vertical.
#' - For bar, rule and tick, this determines whether the size of the bar and tick
#' should be applied to x or y dimension.
#' - For area, this property determines the orient property of the Vega output.
#' - For line and trail marks, this property determines the sort order of the points in the line
#' if `config.sortLineBy` is not specified.
#' For stacked charts, this is always determined by the orientation of the stack;
#' therefore explicitly specified value will be ignored.
#' @param point (_MarkDef_) A flag for overlaying points on top of line or area marks, or an object defining the properties of the overlayed points.
#' 
#' - If this property is `"transparent"`, transparent points will be used (for enhancing tooltips and selections).
#' 
#' - If this property is an empty object (`{}`) or `true`, filled points with default properties will be used.
#' 
#' - If this property is `false`, no points would be automatically added to line or area marks.
#' 
#' __Default value:__ `false`.
#' @param radius (_MarkDef_) Polar coordinate radial offset, in pixels, of the text label from the origin determined by the `x` and `y` properties.
#' @param shape (_MarkDef_) Shape of the point marks. Supported values include:
#' - plotting shapes: `"circle"`, `"square"`, `"cross"`, `"diamond"`, `"triangle-up"`, `"triangle-down"`, `"triangle-right"`, or `"triangle-left"`.
#' - the line symbol `"stroke"`
#' - centered directional shapes `"arrow"`, `"wedge"`, or `"triangle"`
#' - a custom [SVG path string](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) (For correct sizing, custom shape paths should be defined within a square bounding box with coordinates ranging from -1 to 1 along both the x and y dimensions.)
#' 
#' __Default value:__ `"circle"`
#' @param size (_MarkDef_) Default size for marks.
#' - For `point`/`circle`/`square`, this represents the pixel area of the marks. For example: in the case of circles, the radius is determined in part by the square root of the size value.
#' - For `bar`, this represents the band size of the bar, in pixels.
#' - For `text`, this represents the font size, in pixels.
#' 
#' __Default value:__ `30` for point, circle, square marks; `rangeStep` - 1 for bar marks with discrete dimensions; `5` for bar marks with continuous dimensions; `11` for text marks.
#' @param stroke (_MarkDef_) Default Stroke Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param strokeCap (_MarkDef_) The stroke cap for line ending style. One of `"butt"`, `"round"`, or `"square"`.
#' 
#' __Default value:__ `"square"`
#' @param strokeDash (_MarkDef_) An array of alternating stroke, space lengths for creating dashed or dotted lines.
#' @param strokeDashOffset (_MarkDef_) The offset (in pixels) into which to begin drawing with the stroke dash array.
#' @param strokeJoin (_MarkDef_) The stroke line join method. One of `"miter"`, `"round"` or `"bevel"`.
#' 
#' __Default value:__ `"miter"`
#' @param strokeMiterLimit (_MarkDef_) The miter limit at which to bevel a line join.
#' @param strokeOpacity (_MarkDef_) The stroke opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param strokeWidth (_MarkDef_) The stroke width, in pixels.
#' @param style (_MarkDef_) A string or array of strings indicating the name of custom styles to apply to the mark. A style is a named collection of mark property defaults defined within the [style configuration](https://vega.github.io/vega-lite/docs/mark.html#style-config). If style is an array, later styles will override earlier styles. Any [mark properties](https://vega.github.io/vega-lite/docs/encoding.html#mark-prop) explicitly defined within the `encoding` will override a style default.
#' 
#' __Default value:__ The mark's name.  For example, a bar mark will have style `"bar"` by default.
#' __Note:__ Any specified style will augment the default style. For example, a bar mark with `"style": "foo"` will receive from `config.style.bar` and `config.style.foo` (the specified style `"foo"` has higher precedence).
#' @param tension (_MarkDef_) Depending on the interpolation type, sets the tension parameter (for line and area marks).
#' @param text (_MarkDef_) Placeholder text if the `text` channel is not specified
#' @param theta (_MarkDef_) Polar coordinate angle, in radians, of the text label from the origin determined by the `x` and `y` properties. Values for `theta` follow the same convention of `arc` mark `startAngle` and `endAngle` properties: angles are measured in radians, with `0` indicating "north".
#' @param thickness (_MarkDef_) Thickness of the tick mark.
#' 
#' __Default value:__  `1`
#' @param tooltip (_MarkDef_) The tooltip text string to show upon mouse hover or an object defining which fields should the tooltip be derived from.
#' 
#' - If `tooltip` is `{"content": "encoding"}`, then all fields from `encoding` will be used.
#' - If `tooltip` is `{"content": "data"}`, then all fields that appear in the highlighted data point will be used.
#' - If set to `null`, then no tooltip will be used.
#' @param x (_MarkDef_) X coordinates of the marks, or width of horizontal `"bar"` and `"area"` without `x2`.
#' @param x2 (_MarkDef_) X2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param x2Offset (_MarkDef_) Offset for x2-position.
#' @param xOffset (_MarkDef_) Offset for x-position.
#' @param y (_MarkDef_) Y coordinates of the marks, or height of vertical `"bar"` and `"area"` without `y2`
#' @param y2 (_MarkDef_) Y2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param y2Offset (_MarkDef_) Offset for y2-position.
#' @param yOffset (_MarkDef_) Offset for y-position.
#' @return A modified Vega-Lite Spec
#' @export
vl_mark_line <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL){
  args <- .modify_args(list(type = "line"), c("align", "angle", "baseline", "binSpacing", "clip", "color", "cornerRadius", 
  "cursor", "dir", "dx", "dy", "ellipsis", "fill", "fillOpacity", "filled", "font", 
  "fontSize", "fontStyle", "fontWeight", "href", "interpolate", "limit", "line", 
  "opacity", "order", "orient", "point", "radius", "shape", "size", "stroke", 
  "strokeCap", "strokeDash", "strokeDashOffset", "strokeJoin", "strokeMiterLimit", 
  "strokeOpacity", "strokeWidth", "style", "tension", "text", "theta", "thickness", 
  "tooltip", "type", "x", "x2", "x2Offset", "xOffset", "y", "y2", "y2Offset", 
  "yOffset"))
  .add_mark(args$spec, args$object, '#/definitions/MarkDef', args$extra)
} #' vl_mark_trail
#'
#'
#' @param spec An input vega-lite spec
#' @param align (_MarkDef_) The horizontal alignment of the text. One of `"left"`, `"right"`, `"center"`.
#' @param angle (_MarkDef_) The rotation angle of the text, in degrees.
#' @param baseline (_MarkDef_) The vertical alignment of the text. One of `"top"`, `"middle"`, `"bottom"`.
#' 
#' __Default value:__ `"middle"`
#' @param binSpacing (_MarkDef_) Offset between bars for binned field.  Ideal value for this is either 0 (Preferred by statisticians) or 1 (Vega-Lite Default, D3 example style).
#' 
#' __Default value:__ `1`
#' @param clip (_MarkDef_) Whether a mark be clipped to the enclosing group’s width and height.
#' @param color (_MarkDef_) Default color.  Note that `fill` and `stroke` have higher precedence than `color` and will override `color`.
#' 
#' __Default value:__ <span style="color: #4682b4;">&#9632;</span> `"#4682b4"`
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param cornerRadius (_MarkDef_) The radius in pixels of rounded rectangle corners.
#' 
#' __Default value:__ `0`
#' @param cursor (_MarkDef_) The mouse cursor used over the mark. Any valid [CSS cursor type](https://developer.mozilla.org/en-US/docs/Web/CSS/cursor#Values) can be used.
#' @param dir (_MarkDef_) The direction of the text. One of `"ltr"` (left-to-right) or `"rtl"` (right-to-left). This property determines on which side is truncated in response to the limit parameter.
#' 
#' __Default value:__ `"ltr"`
#' @param dx (_MarkDef_) The horizontal offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param dy (_MarkDef_) The vertical offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param ellipsis (_MarkDef_) The ellipsis string for text truncated in response to the limit parameter.
#' 
#' __Default value:__ `"…"`
#' @param fill (_MarkDef_) Default Fill Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param fillOpacity (_MarkDef_) The fill opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param filled (_MarkDef_) Whether the mark's color should be used as fill color instead of stroke color.
#' 
#' __Default value:__ `false` for `point`, `line` and `rule`; otherwise, `true`.
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param font (_MarkDef_) The typeface to set the text in (e.g., `"Helvetica Neue"`).
#' @param fontSize (_MarkDef_) The font size, in pixels.
#' @param fontStyle (_MarkDef_) The font style (e.g., `"italic"`).
#' @param fontWeight (_MarkDef_) The font weight.
#' This can be either a string (e.g `"bold"`, `"normal"`) or a number (`100`, `200`, `300`, ..., `900` where `"normal"` = `400` and `"bold"` = `700`).
#' @param href (_MarkDef_) A URL to load upon mouse click. If defined, the mark acts as a hyperlink.
#' @param interpolate (_MarkDef_) The line interpolation method to use for line and area marks. One of the following:
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
#' - `"monotone"`: cubic interpolation that preserves monotonicity in y.
#' @param limit (_MarkDef_) The maximum length of the text mark in pixels. The text value will be automatically truncated if the rendered size exceeds the limit.
#' 
#' __Default value:__ `0`, indicating no limit
#' @param line (_MarkDef_) A flag for overlaying line on top of area marks, or an object defining the properties of the overlayed lines.
#' 
#' - If this value is an empty object (`{}`) or `true`, lines with default properties will be used.
#' 
#' - If this value is `false`, no lines would be automatically added to area marks.
#' 
#' __Default value:__ `false`.
#' @param opacity (_MarkDef_) The overall opacity (value between \[0,1\]).
#' 
#' __Default value:__ `0.7` for non-aggregate plots with `point`, `tick`, `circle`, or `square` marks or layered `bar` charts and `1` otherwise.
#' @param order (_MarkDef_) For line and trail marks, this `order` property can be set to `null` or `false` to make the lines use the original order in the data sources.
#' @param orient (_MarkDef_) The orientation of a non-stacked bar, tick, area, and line charts.
#' The value is either horizontal (default) or vertical.
#' - For bar, rule and tick, this determines whether the size of the bar and tick
#' should be applied to x or y dimension.
#' - For area, this property determines the orient property of the Vega output.
#' - For line and trail marks, this property determines the sort order of the points in the line
#' if `config.sortLineBy` is not specified.
#' For stacked charts, this is always determined by the orientation of the stack;
#' therefore explicitly specified value will be ignored.
#' @param point (_MarkDef_) A flag for overlaying points on top of line or area marks, or an object defining the properties of the overlayed points.
#' 
#' - If this property is `"transparent"`, transparent points will be used (for enhancing tooltips and selections).
#' 
#' - If this property is an empty object (`{}`) or `true`, filled points with default properties will be used.
#' 
#' - If this property is `false`, no points would be automatically added to line or area marks.
#' 
#' __Default value:__ `false`.
#' @param radius (_MarkDef_) Polar coordinate radial offset, in pixels, of the text label from the origin determined by the `x` and `y` properties.
#' @param shape (_MarkDef_) Shape of the point marks. Supported values include:
#' - plotting shapes: `"circle"`, `"square"`, `"cross"`, `"diamond"`, `"triangle-up"`, `"triangle-down"`, `"triangle-right"`, or `"triangle-left"`.
#' - the line symbol `"stroke"`
#' - centered directional shapes `"arrow"`, `"wedge"`, or `"triangle"`
#' - a custom [SVG path string](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) (For correct sizing, custom shape paths should be defined within a square bounding box with coordinates ranging from -1 to 1 along both the x and y dimensions.)
#' 
#' __Default value:__ `"circle"`
#' @param size (_MarkDef_) Default size for marks.
#' - For `point`/`circle`/`square`, this represents the pixel area of the marks. For example: in the case of circles, the radius is determined in part by the square root of the size value.
#' - For `bar`, this represents the band size of the bar, in pixels.
#' - For `text`, this represents the font size, in pixels.
#' 
#' __Default value:__ `30` for point, circle, square marks; `rangeStep` - 1 for bar marks with discrete dimensions; `5` for bar marks with continuous dimensions; `11` for text marks.
#' @param stroke (_MarkDef_) Default Stroke Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param strokeCap (_MarkDef_) The stroke cap for line ending style. One of `"butt"`, `"round"`, or `"square"`.
#' 
#' __Default value:__ `"square"`
#' @param strokeDash (_MarkDef_) An array of alternating stroke, space lengths for creating dashed or dotted lines.
#' @param strokeDashOffset (_MarkDef_) The offset (in pixels) into which to begin drawing with the stroke dash array.
#' @param strokeJoin (_MarkDef_) The stroke line join method. One of `"miter"`, `"round"` or `"bevel"`.
#' 
#' __Default value:__ `"miter"`
#' @param strokeMiterLimit (_MarkDef_) The miter limit at which to bevel a line join.
#' @param strokeOpacity (_MarkDef_) The stroke opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param strokeWidth (_MarkDef_) The stroke width, in pixels.
#' @param style (_MarkDef_) A string or array of strings indicating the name of custom styles to apply to the mark. A style is a named collection of mark property defaults defined within the [style configuration](https://vega.github.io/vega-lite/docs/mark.html#style-config). If style is an array, later styles will override earlier styles. Any [mark properties](https://vega.github.io/vega-lite/docs/encoding.html#mark-prop) explicitly defined within the `encoding` will override a style default.
#' 
#' __Default value:__ The mark's name.  For example, a bar mark will have style `"bar"` by default.
#' __Note:__ Any specified style will augment the default style. For example, a bar mark with `"style": "foo"` will receive from `config.style.bar` and `config.style.foo` (the specified style `"foo"` has higher precedence).
#' @param tension (_MarkDef_) Depending on the interpolation type, sets the tension parameter (for line and area marks).
#' @param text (_MarkDef_) Placeholder text if the `text` channel is not specified
#' @param theta (_MarkDef_) Polar coordinate angle, in radians, of the text label from the origin determined by the `x` and `y` properties. Values for `theta` follow the same convention of `arc` mark `startAngle` and `endAngle` properties: angles are measured in radians, with `0` indicating "north".
#' @param thickness (_MarkDef_) Thickness of the tick mark.
#' 
#' __Default value:__  `1`
#' @param tooltip (_MarkDef_) The tooltip text string to show upon mouse hover or an object defining which fields should the tooltip be derived from.
#' 
#' - If `tooltip` is `{"content": "encoding"}`, then all fields from `encoding` will be used.
#' - If `tooltip` is `{"content": "data"}`, then all fields that appear in the highlighted data point will be used.
#' - If set to `null`, then no tooltip will be used.
#' @param x (_MarkDef_) X coordinates of the marks, or width of horizontal `"bar"` and `"area"` without `x2`.
#' @param x2 (_MarkDef_) X2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param x2Offset (_MarkDef_) Offset for x2-position.
#' @param xOffset (_MarkDef_) Offset for x-position.
#' @param y (_MarkDef_) Y coordinates of the marks, or height of vertical `"bar"` and `"area"` without `y2`
#' @param y2 (_MarkDef_) Y2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param y2Offset (_MarkDef_) Offset for y2-position.
#' @param yOffset (_MarkDef_) Offset for y-position.
#' @return A modified Vega-Lite Spec
#' @export
vl_mark_trail <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL){
  args <- .modify_args(list(type = "trail"), c("align", "angle", "baseline", "binSpacing", "clip", "color", "cornerRadius", 
  "cursor", "dir", "dx", "dy", "ellipsis", "fill", "fillOpacity", "filled", "font", 
  "fontSize", "fontStyle", "fontWeight", "href", "interpolate", "limit", "line", 
  "opacity", "order", "orient", "point", "radius", "shape", "size", "stroke", 
  "strokeCap", "strokeDash", "strokeDashOffset", "strokeJoin", "strokeMiterLimit", 
  "strokeOpacity", "strokeWidth", "style", "tension", "text", "theta", "thickness", 
  "tooltip", "type", "x", "x2", "x2Offset", "xOffset", "y", "y2", "y2Offset", 
  "yOffset"))
  .add_mark(args$spec, args$object, '#/definitions/MarkDef', args$extra)
} #' vl_mark_point
#'
#'
#' @param spec An input vega-lite spec
#' @param align (_MarkDef_) The horizontal alignment of the text. One of `"left"`, `"right"`, `"center"`.
#' @param angle (_MarkDef_) The rotation angle of the text, in degrees.
#' @param baseline (_MarkDef_) The vertical alignment of the text. One of `"top"`, `"middle"`, `"bottom"`.
#' 
#' __Default value:__ `"middle"`
#' @param binSpacing (_MarkDef_) Offset between bars for binned field.  Ideal value for this is either 0 (Preferred by statisticians) or 1 (Vega-Lite Default, D3 example style).
#' 
#' __Default value:__ `1`
#' @param clip (_MarkDef_) Whether a mark be clipped to the enclosing group’s width and height.
#' @param color (_MarkDef_) Default color.  Note that `fill` and `stroke` have higher precedence than `color` and will override `color`.
#' 
#' __Default value:__ <span style="color: #4682b4;">&#9632;</span> `"#4682b4"`
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param cornerRadius (_MarkDef_) The radius in pixels of rounded rectangle corners.
#' 
#' __Default value:__ `0`
#' @param cursor (_MarkDef_) The mouse cursor used over the mark. Any valid [CSS cursor type](https://developer.mozilla.org/en-US/docs/Web/CSS/cursor#Values) can be used.
#' @param dir (_MarkDef_) The direction of the text. One of `"ltr"` (left-to-right) or `"rtl"` (right-to-left). This property determines on which side is truncated in response to the limit parameter.
#' 
#' __Default value:__ `"ltr"`
#' @param dx (_MarkDef_) The horizontal offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param dy (_MarkDef_) The vertical offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param ellipsis (_MarkDef_) The ellipsis string for text truncated in response to the limit parameter.
#' 
#' __Default value:__ `"…"`
#' @param fill (_MarkDef_) Default Fill Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param fillOpacity (_MarkDef_) The fill opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param filled (_MarkDef_) Whether the mark's color should be used as fill color instead of stroke color.
#' 
#' __Default value:__ `false` for `point`, `line` and `rule`; otherwise, `true`.
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param font (_MarkDef_) The typeface to set the text in (e.g., `"Helvetica Neue"`).
#' @param fontSize (_MarkDef_) The font size, in pixels.
#' @param fontStyle (_MarkDef_) The font style (e.g., `"italic"`).
#' @param fontWeight (_MarkDef_) The font weight.
#' This can be either a string (e.g `"bold"`, `"normal"`) or a number (`100`, `200`, `300`, ..., `900` where `"normal"` = `400` and `"bold"` = `700`).
#' @param href (_MarkDef_) A URL to load upon mouse click. If defined, the mark acts as a hyperlink.
#' @param interpolate (_MarkDef_) The line interpolation method to use for line and area marks. One of the following:
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
#' - `"monotone"`: cubic interpolation that preserves monotonicity in y.
#' @param limit (_MarkDef_) The maximum length of the text mark in pixels. The text value will be automatically truncated if the rendered size exceeds the limit.
#' 
#' __Default value:__ `0`, indicating no limit
#' @param line (_MarkDef_) A flag for overlaying line on top of area marks, or an object defining the properties of the overlayed lines.
#' 
#' - If this value is an empty object (`{}`) or `true`, lines with default properties will be used.
#' 
#' - If this value is `false`, no lines would be automatically added to area marks.
#' 
#' __Default value:__ `false`.
#' @param opacity (_MarkDef_) The overall opacity (value between \[0,1\]).
#' 
#' __Default value:__ `0.7` for non-aggregate plots with `point`, `tick`, `circle`, or `square` marks or layered `bar` charts and `1` otherwise.
#' @param order (_MarkDef_) For line and trail marks, this `order` property can be set to `null` or `false` to make the lines use the original order in the data sources.
#' @param orient (_MarkDef_) The orientation of a non-stacked bar, tick, area, and line charts.
#' The value is either horizontal (default) or vertical.
#' - For bar, rule and tick, this determines whether the size of the bar and tick
#' should be applied to x or y dimension.
#' - For area, this property determines the orient property of the Vega output.
#' - For line and trail marks, this property determines the sort order of the points in the line
#' if `config.sortLineBy` is not specified.
#' For stacked charts, this is always determined by the orientation of the stack;
#' therefore explicitly specified value will be ignored.
#' @param point (_MarkDef_) A flag for overlaying points on top of line or area marks, or an object defining the properties of the overlayed points.
#' 
#' - If this property is `"transparent"`, transparent points will be used (for enhancing tooltips and selections).
#' 
#' - If this property is an empty object (`{}`) or `true`, filled points with default properties will be used.
#' 
#' - If this property is `false`, no points would be automatically added to line or area marks.
#' 
#' __Default value:__ `false`.
#' @param radius (_MarkDef_) Polar coordinate radial offset, in pixels, of the text label from the origin determined by the `x` and `y` properties.
#' @param shape (_MarkDef_) Shape of the point marks. Supported values include:
#' - plotting shapes: `"circle"`, `"square"`, `"cross"`, `"diamond"`, `"triangle-up"`, `"triangle-down"`, `"triangle-right"`, or `"triangle-left"`.
#' - the line symbol `"stroke"`
#' - centered directional shapes `"arrow"`, `"wedge"`, or `"triangle"`
#' - a custom [SVG path string](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) (For correct sizing, custom shape paths should be defined within a square bounding box with coordinates ranging from -1 to 1 along both the x and y dimensions.)
#' 
#' __Default value:__ `"circle"`
#' @param size (_MarkDef_) Default size for marks.
#' - For `point`/`circle`/`square`, this represents the pixel area of the marks. For example: in the case of circles, the radius is determined in part by the square root of the size value.
#' - For `bar`, this represents the band size of the bar, in pixels.
#' - For `text`, this represents the font size, in pixels.
#' 
#' __Default value:__ `30` for point, circle, square marks; `rangeStep` - 1 for bar marks with discrete dimensions; `5` for bar marks with continuous dimensions; `11` for text marks.
#' @param stroke (_MarkDef_) Default Stroke Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param strokeCap (_MarkDef_) The stroke cap for line ending style. One of `"butt"`, `"round"`, or `"square"`.
#' 
#' __Default value:__ `"square"`
#' @param strokeDash (_MarkDef_) An array of alternating stroke, space lengths for creating dashed or dotted lines.
#' @param strokeDashOffset (_MarkDef_) The offset (in pixels) into which to begin drawing with the stroke dash array.
#' @param strokeJoin (_MarkDef_) The stroke line join method. One of `"miter"`, `"round"` or `"bevel"`.
#' 
#' __Default value:__ `"miter"`
#' @param strokeMiterLimit (_MarkDef_) The miter limit at which to bevel a line join.
#' @param strokeOpacity (_MarkDef_) The stroke opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param strokeWidth (_MarkDef_) The stroke width, in pixels.
#' @param style (_MarkDef_) A string or array of strings indicating the name of custom styles to apply to the mark. A style is a named collection of mark property defaults defined within the [style configuration](https://vega.github.io/vega-lite/docs/mark.html#style-config). If style is an array, later styles will override earlier styles. Any [mark properties](https://vega.github.io/vega-lite/docs/encoding.html#mark-prop) explicitly defined within the `encoding` will override a style default.
#' 
#' __Default value:__ The mark's name.  For example, a bar mark will have style `"bar"` by default.
#' __Note:__ Any specified style will augment the default style. For example, a bar mark with `"style": "foo"` will receive from `config.style.bar` and `config.style.foo` (the specified style `"foo"` has higher precedence).
#' @param tension (_MarkDef_) Depending on the interpolation type, sets the tension parameter (for line and area marks).
#' @param text (_MarkDef_) Placeholder text if the `text` channel is not specified
#' @param theta (_MarkDef_) Polar coordinate angle, in radians, of the text label from the origin determined by the `x` and `y` properties. Values for `theta` follow the same convention of `arc` mark `startAngle` and `endAngle` properties: angles are measured in radians, with `0` indicating "north".
#' @param thickness (_MarkDef_) Thickness of the tick mark.
#' 
#' __Default value:__  `1`
#' @param tooltip (_MarkDef_) The tooltip text string to show upon mouse hover or an object defining which fields should the tooltip be derived from.
#' 
#' - If `tooltip` is `{"content": "encoding"}`, then all fields from `encoding` will be used.
#' - If `tooltip` is `{"content": "data"}`, then all fields that appear in the highlighted data point will be used.
#' - If set to `null`, then no tooltip will be used.
#' @param x (_MarkDef_) X coordinates of the marks, or width of horizontal `"bar"` and `"area"` without `x2`.
#' @param x2 (_MarkDef_) X2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param x2Offset (_MarkDef_) Offset for x2-position.
#' @param xOffset (_MarkDef_) Offset for x-position.
#' @param y (_MarkDef_) Y coordinates of the marks, or height of vertical `"bar"` and `"area"` without `y2`
#' @param y2 (_MarkDef_) Y2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param y2Offset (_MarkDef_) Offset for y2-position.
#' @param yOffset (_MarkDef_) Offset for y-position.
#' @return A modified Vega-Lite Spec
#' @export
vl_mark_point <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL){
  args <- .modify_args(list(type = "point"), c("align", "angle", "baseline", "binSpacing", "clip", "color", "cornerRadius", 
  "cursor", "dir", "dx", "dy", "ellipsis", "fill", "fillOpacity", "filled", "font", 
  "fontSize", "fontStyle", "fontWeight", "href", "interpolate", "limit", "line", 
  "opacity", "order", "orient", "point", "radius", "shape", "size", "stroke", 
  "strokeCap", "strokeDash", "strokeDashOffset", "strokeJoin", "strokeMiterLimit", 
  "strokeOpacity", "strokeWidth", "style", "tension", "text", "theta", "thickness", 
  "tooltip", "type", "x", "x2", "x2Offset", "xOffset", "y", "y2", "y2Offset", 
  "yOffset"))
  .add_mark(args$spec, args$object, '#/definitions/MarkDef', args$extra)
} #' vl_mark_text
#'
#'
#' @param spec An input vega-lite spec
#' @param align (_MarkDef_) The horizontal alignment of the text. One of `"left"`, `"right"`, `"center"`.
#' @param angle (_MarkDef_) The rotation angle of the text, in degrees.
#' @param baseline (_MarkDef_) The vertical alignment of the text. One of `"top"`, `"middle"`, `"bottom"`.
#' 
#' __Default value:__ `"middle"`
#' @param binSpacing (_MarkDef_) Offset between bars for binned field.  Ideal value for this is either 0 (Preferred by statisticians) or 1 (Vega-Lite Default, D3 example style).
#' 
#' __Default value:__ `1`
#' @param clip (_MarkDef_) Whether a mark be clipped to the enclosing group’s width and height.
#' @param color (_MarkDef_) Default color.  Note that `fill` and `stroke` have higher precedence than `color` and will override `color`.
#' 
#' __Default value:__ <span style="color: #4682b4;">&#9632;</span> `"#4682b4"`
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param cornerRadius (_MarkDef_) The radius in pixels of rounded rectangle corners.
#' 
#' __Default value:__ `0`
#' @param cursor (_MarkDef_) The mouse cursor used over the mark. Any valid [CSS cursor type](https://developer.mozilla.org/en-US/docs/Web/CSS/cursor#Values) can be used.
#' @param dir (_MarkDef_) The direction of the text. One of `"ltr"` (left-to-right) or `"rtl"` (right-to-left). This property determines on which side is truncated in response to the limit parameter.
#' 
#' __Default value:__ `"ltr"`
#' @param dx (_MarkDef_) The horizontal offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param dy (_MarkDef_) The vertical offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param ellipsis (_MarkDef_) The ellipsis string for text truncated in response to the limit parameter.
#' 
#' __Default value:__ `"…"`
#' @param fill (_MarkDef_) Default Fill Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param fillOpacity (_MarkDef_) The fill opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param filled (_MarkDef_) Whether the mark's color should be used as fill color instead of stroke color.
#' 
#' __Default value:__ `false` for `point`, `line` and `rule`; otherwise, `true`.
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param font (_MarkDef_) The typeface to set the text in (e.g., `"Helvetica Neue"`).
#' @param fontSize (_MarkDef_) The font size, in pixels.
#' @param fontStyle (_MarkDef_) The font style (e.g., `"italic"`).
#' @param fontWeight (_MarkDef_) The font weight.
#' This can be either a string (e.g `"bold"`, `"normal"`) or a number (`100`, `200`, `300`, ..., `900` where `"normal"` = `400` and `"bold"` = `700`).
#' @param href (_MarkDef_) A URL to load upon mouse click. If defined, the mark acts as a hyperlink.
#' @param interpolate (_MarkDef_) The line interpolation method to use for line and area marks. One of the following:
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
#' - `"monotone"`: cubic interpolation that preserves monotonicity in y.
#' @param limit (_MarkDef_) The maximum length of the text mark in pixels. The text value will be automatically truncated if the rendered size exceeds the limit.
#' 
#' __Default value:__ `0`, indicating no limit
#' @param line (_MarkDef_) A flag for overlaying line on top of area marks, or an object defining the properties of the overlayed lines.
#' 
#' - If this value is an empty object (`{}`) or `true`, lines with default properties will be used.
#' 
#' - If this value is `false`, no lines would be automatically added to area marks.
#' 
#' __Default value:__ `false`.
#' @param opacity (_MarkDef_) The overall opacity (value between \[0,1\]).
#' 
#' __Default value:__ `0.7` for non-aggregate plots with `point`, `tick`, `circle`, or `square` marks or layered `bar` charts and `1` otherwise.
#' @param order (_MarkDef_) For line and trail marks, this `order` property can be set to `null` or `false` to make the lines use the original order in the data sources.
#' @param orient (_MarkDef_) The orientation of a non-stacked bar, tick, area, and line charts.
#' The value is either horizontal (default) or vertical.
#' - For bar, rule and tick, this determines whether the size of the bar and tick
#' should be applied to x or y dimension.
#' - For area, this property determines the orient property of the Vega output.
#' - For line and trail marks, this property determines the sort order of the points in the line
#' if `config.sortLineBy` is not specified.
#' For stacked charts, this is always determined by the orientation of the stack;
#' therefore explicitly specified value will be ignored.
#' @param point (_MarkDef_) A flag for overlaying points on top of line or area marks, or an object defining the properties of the overlayed points.
#' 
#' - If this property is `"transparent"`, transparent points will be used (for enhancing tooltips and selections).
#' 
#' - If this property is an empty object (`{}`) or `true`, filled points with default properties will be used.
#' 
#' - If this property is `false`, no points would be automatically added to line or area marks.
#' 
#' __Default value:__ `false`.
#' @param radius (_MarkDef_) Polar coordinate radial offset, in pixels, of the text label from the origin determined by the `x` and `y` properties.
#' @param shape (_MarkDef_) Shape of the point marks. Supported values include:
#' - plotting shapes: `"circle"`, `"square"`, `"cross"`, `"diamond"`, `"triangle-up"`, `"triangle-down"`, `"triangle-right"`, or `"triangle-left"`.
#' - the line symbol `"stroke"`
#' - centered directional shapes `"arrow"`, `"wedge"`, or `"triangle"`
#' - a custom [SVG path string](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) (For correct sizing, custom shape paths should be defined within a square bounding box with coordinates ranging from -1 to 1 along both the x and y dimensions.)
#' 
#' __Default value:__ `"circle"`
#' @param size (_MarkDef_) Default size for marks.
#' - For `point`/`circle`/`square`, this represents the pixel area of the marks. For example: in the case of circles, the radius is determined in part by the square root of the size value.
#' - For `bar`, this represents the band size of the bar, in pixels.
#' - For `text`, this represents the font size, in pixels.
#' 
#' __Default value:__ `30` for point, circle, square marks; `rangeStep` - 1 for bar marks with discrete dimensions; `5` for bar marks with continuous dimensions; `11` for text marks.
#' @param stroke (_MarkDef_) Default Stroke Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param strokeCap (_MarkDef_) The stroke cap for line ending style. One of `"butt"`, `"round"`, or `"square"`.
#' 
#' __Default value:__ `"square"`
#' @param strokeDash (_MarkDef_) An array of alternating stroke, space lengths for creating dashed or dotted lines.
#' @param strokeDashOffset (_MarkDef_) The offset (in pixels) into which to begin drawing with the stroke dash array.
#' @param strokeJoin (_MarkDef_) The stroke line join method. One of `"miter"`, `"round"` or `"bevel"`.
#' 
#' __Default value:__ `"miter"`
#' @param strokeMiterLimit (_MarkDef_) The miter limit at which to bevel a line join.
#' @param strokeOpacity (_MarkDef_) The stroke opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param strokeWidth (_MarkDef_) The stroke width, in pixels.
#' @param style (_MarkDef_) A string or array of strings indicating the name of custom styles to apply to the mark. A style is a named collection of mark property defaults defined within the [style configuration](https://vega.github.io/vega-lite/docs/mark.html#style-config). If style is an array, later styles will override earlier styles. Any [mark properties](https://vega.github.io/vega-lite/docs/encoding.html#mark-prop) explicitly defined within the `encoding` will override a style default.
#' 
#' __Default value:__ The mark's name.  For example, a bar mark will have style `"bar"` by default.
#' __Note:__ Any specified style will augment the default style. For example, a bar mark with `"style": "foo"` will receive from `config.style.bar` and `config.style.foo` (the specified style `"foo"` has higher precedence).
#' @param tension (_MarkDef_) Depending on the interpolation type, sets the tension parameter (for line and area marks).
#' @param text (_MarkDef_) Placeholder text if the `text` channel is not specified
#' @param theta (_MarkDef_) Polar coordinate angle, in radians, of the text label from the origin determined by the `x` and `y` properties. Values for `theta` follow the same convention of `arc` mark `startAngle` and `endAngle` properties: angles are measured in radians, with `0` indicating "north".
#' @param thickness (_MarkDef_) Thickness of the tick mark.
#' 
#' __Default value:__  `1`
#' @param tooltip (_MarkDef_) The tooltip text string to show upon mouse hover or an object defining which fields should the tooltip be derived from.
#' 
#' - If `tooltip` is `{"content": "encoding"}`, then all fields from `encoding` will be used.
#' - If `tooltip` is `{"content": "data"}`, then all fields that appear in the highlighted data point will be used.
#' - If set to `null`, then no tooltip will be used.
#' @param x (_MarkDef_) X coordinates of the marks, or width of horizontal `"bar"` and `"area"` without `x2`.
#' @param x2 (_MarkDef_) X2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param x2Offset (_MarkDef_) Offset for x2-position.
#' @param xOffset (_MarkDef_) Offset for x-position.
#' @param y (_MarkDef_) Y coordinates of the marks, or height of vertical `"bar"` and `"area"` without `y2`
#' @param y2 (_MarkDef_) Y2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param y2Offset (_MarkDef_) Offset for y2-position.
#' @param yOffset (_MarkDef_) Offset for y-position.
#' @return A modified Vega-Lite Spec
#' @export
vl_mark_text <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL){
  args <- .modify_args(list(type = "text"), c("align", "angle", "baseline", "binSpacing", "clip", "color", "cornerRadius", 
  "cursor", "dir", "dx", "dy", "ellipsis", "fill", "fillOpacity", "filled", "font", 
  "fontSize", "fontStyle", "fontWeight", "href", "interpolate", "limit", "line", 
  "opacity", "order", "orient", "point", "radius", "shape", "size", "stroke", 
  "strokeCap", "strokeDash", "strokeDashOffset", "strokeJoin", "strokeMiterLimit", 
  "strokeOpacity", "strokeWidth", "style", "tension", "text", "theta", "thickness", 
  "tooltip", "type", "x", "x2", "x2Offset", "xOffset", "y", "y2", "y2Offset", 
  "yOffset"))
  .add_mark(args$spec, args$object, '#/definitions/MarkDef', args$extra)
} #' vl_mark_tick
#'
#'
#' @param spec An input vega-lite spec
#' @param align (_MarkDef_) The horizontal alignment of the text. One of `"left"`, `"right"`, `"center"`.
#' @param angle (_MarkDef_) The rotation angle of the text, in degrees.
#' @param baseline (_MarkDef_) The vertical alignment of the text. One of `"top"`, `"middle"`, `"bottom"`.
#' 
#' __Default value:__ `"middle"`
#' @param binSpacing (_MarkDef_) Offset between bars for binned field.  Ideal value for this is either 0 (Preferred by statisticians) or 1 (Vega-Lite Default, D3 example style).
#' 
#' __Default value:__ `1`
#' @param clip (_MarkDef_) Whether a mark be clipped to the enclosing group’s width and height.
#' @param color (_MarkDef_) Default color.  Note that `fill` and `stroke` have higher precedence than `color` and will override `color`.
#' 
#' __Default value:__ <span style="color: #4682b4;">&#9632;</span> `"#4682b4"`
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param cornerRadius (_MarkDef_) The radius in pixels of rounded rectangle corners.
#' 
#' __Default value:__ `0`
#' @param cursor (_MarkDef_) The mouse cursor used over the mark. Any valid [CSS cursor type](https://developer.mozilla.org/en-US/docs/Web/CSS/cursor#Values) can be used.
#' @param dir (_MarkDef_) The direction of the text. One of `"ltr"` (left-to-right) or `"rtl"` (right-to-left). This property determines on which side is truncated in response to the limit parameter.
#' 
#' __Default value:__ `"ltr"`
#' @param dx (_MarkDef_) The horizontal offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param dy (_MarkDef_) The vertical offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param ellipsis (_MarkDef_) The ellipsis string for text truncated in response to the limit parameter.
#' 
#' __Default value:__ `"…"`
#' @param fill (_MarkDef_) Default Fill Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param fillOpacity (_MarkDef_) The fill opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param filled (_MarkDef_) Whether the mark's color should be used as fill color instead of stroke color.
#' 
#' __Default value:__ `false` for `point`, `line` and `rule`; otherwise, `true`.
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param font (_MarkDef_) The typeface to set the text in (e.g., `"Helvetica Neue"`).
#' @param fontSize (_MarkDef_) The font size, in pixels.
#' @param fontStyle (_MarkDef_) The font style (e.g., `"italic"`).
#' @param fontWeight (_MarkDef_) The font weight.
#' This can be either a string (e.g `"bold"`, `"normal"`) or a number (`100`, `200`, `300`, ..., `900` where `"normal"` = `400` and `"bold"` = `700`).
#' @param href (_MarkDef_) A URL to load upon mouse click. If defined, the mark acts as a hyperlink.
#' @param interpolate (_MarkDef_) The line interpolation method to use for line and area marks. One of the following:
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
#' - `"monotone"`: cubic interpolation that preserves monotonicity in y.
#' @param limit (_MarkDef_) The maximum length of the text mark in pixels. The text value will be automatically truncated if the rendered size exceeds the limit.
#' 
#' __Default value:__ `0`, indicating no limit
#' @param line (_MarkDef_) A flag for overlaying line on top of area marks, or an object defining the properties of the overlayed lines.
#' 
#' - If this value is an empty object (`{}`) or `true`, lines with default properties will be used.
#' 
#' - If this value is `false`, no lines would be automatically added to area marks.
#' 
#' __Default value:__ `false`.
#' @param opacity (_MarkDef_) The overall opacity (value between \[0,1\]).
#' 
#' __Default value:__ `0.7` for non-aggregate plots with `point`, `tick`, `circle`, or `square` marks or layered `bar` charts and `1` otherwise.
#' @param order (_MarkDef_) For line and trail marks, this `order` property can be set to `null` or `false` to make the lines use the original order in the data sources.
#' @param orient (_MarkDef_) The orientation of a non-stacked bar, tick, area, and line charts.
#' The value is either horizontal (default) or vertical.
#' - For bar, rule and tick, this determines whether the size of the bar and tick
#' should be applied to x or y dimension.
#' - For area, this property determines the orient property of the Vega output.
#' - For line and trail marks, this property determines the sort order of the points in the line
#' if `config.sortLineBy` is not specified.
#' For stacked charts, this is always determined by the orientation of the stack;
#' therefore explicitly specified value will be ignored.
#' @param point (_MarkDef_) A flag for overlaying points on top of line or area marks, or an object defining the properties of the overlayed points.
#' 
#' - If this property is `"transparent"`, transparent points will be used (for enhancing tooltips and selections).
#' 
#' - If this property is an empty object (`{}`) or `true`, filled points with default properties will be used.
#' 
#' - If this property is `false`, no points would be automatically added to line or area marks.
#' 
#' __Default value:__ `false`.
#' @param radius (_MarkDef_) Polar coordinate radial offset, in pixels, of the text label from the origin determined by the `x` and `y` properties.
#' @param shape (_MarkDef_) Shape of the point marks. Supported values include:
#' - plotting shapes: `"circle"`, `"square"`, `"cross"`, `"diamond"`, `"triangle-up"`, `"triangle-down"`, `"triangle-right"`, or `"triangle-left"`.
#' - the line symbol `"stroke"`
#' - centered directional shapes `"arrow"`, `"wedge"`, or `"triangle"`
#' - a custom [SVG path string](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) (For correct sizing, custom shape paths should be defined within a square bounding box with coordinates ranging from -1 to 1 along both the x and y dimensions.)
#' 
#' __Default value:__ `"circle"`
#' @param size (_MarkDef_) Default size for marks.
#' - For `point`/`circle`/`square`, this represents the pixel area of the marks. For example: in the case of circles, the radius is determined in part by the square root of the size value.
#' - For `bar`, this represents the band size of the bar, in pixels.
#' - For `text`, this represents the font size, in pixels.
#' 
#' __Default value:__ `30` for point, circle, square marks; `rangeStep` - 1 for bar marks with discrete dimensions; `5` for bar marks with continuous dimensions; `11` for text marks.
#' @param stroke (_MarkDef_) Default Stroke Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param strokeCap (_MarkDef_) The stroke cap for line ending style. One of `"butt"`, `"round"`, or `"square"`.
#' 
#' __Default value:__ `"square"`
#' @param strokeDash (_MarkDef_) An array of alternating stroke, space lengths for creating dashed or dotted lines.
#' @param strokeDashOffset (_MarkDef_) The offset (in pixels) into which to begin drawing with the stroke dash array.
#' @param strokeJoin (_MarkDef_) The stroke line join method. One of `"miter"`, `"round"` or `"bevel"`.
#' 
#' __Default value:__ `"miter"`
#' @param strokeMiterLimit (_MarkDef_) The miter limit at which to bevel a line join.
#' @param strokeOpacity (_MarkDef_) The stroke opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param strokeWidth (_MarkDef_) The stroke width, in pixels.
#' @param style (_MarkDef_) A string or array of strings indicating the name of custom styles to apply to the mark. A style is a named collection of mark property defaults defined within the [style configuration](https://vega.github.io/vega-lite/docs/mark.html#style-config). If style is an array, later styles will override earlier styles. Any [mark properties](https://vega.github.io/vega-lite/docs/encoding.html#mark-prop) explicitly defined within the `encoding` will override a style default.
#' 
#' __Default value:__ The mark's name.  For example, a bar mark will have style `"bar"` by default.
#' __Note:__ Any specified style will augment the default style. For example, a bar mark with `"style": "foo"` will receive from `config.style.bar` and `config.style.foo` (the specified style `"foo"` has higher precedence).
#' @param tension (_MarkDef_) Depending on the interpolation type, sets the tension parameter (for line and area marks).
#' @param text (_MarkDef_) Placeholder text if the `text` channel is not specified
#' @param theta (_MarkDef_) Polar coordinate angle, in radians, of the text label from the origin determined by the `x` and `y` properties. Values for `theta` follow the same convention of `arc` mark `startAngle` and `endAngle` properties: angles are measured in radians, with `0` indicating "north".
#' @param thickness (_MarkDef_) Thickness of the tick mark.
#' 
#' __Default value:__  `1`
#' @param tooltip (_MarkDef_) The tooltip text string to show upon mouse hover or an object defining which fields should the tooltip be derived from.
#' 
#' - If `tooltip` is `{"content": "encoding"}`, then all fields from `encoding` will be used.
#' - If `tooltip` is `{"content": "data"}`, then all fields that appear in the highlighted data point will be used.
#' - If set to `null`, then no tooltip will be used.
#' @param x (_MarkDef_) X coordinates of the marks, or width of horizontal `"bar"` and `"area"` without `x2`.
#' @param x2 (_MarkDef_) X2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param x2Offset (_MarkDef_) Offset for x2-position.
#' @param xOffset (_MarkDef_) Offset for x-position.
#' @param y (_MarkDef_) Y coordinates of the marks, or height of vertical `"bar"` and `"area"` without `y2`
#' @param y2 (_MarkDef_) Y2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param y2Offset (_MarkDef_) Offset for y2-position.
#' @param yOffset (_MarkDef_) Offset for y-position.
#' @return A modified Vega-Lite Spec
#' @export
vl_mark_tick <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL){
  args <- .modify_args(list(type = "tick"), c("align", "angle", "baseline", "binSpacing", "clip", "color", "cornerRadius", 
  "cursor", "dir", "dx", "dy", "ellipsis", "fill", "fillOpacity", "filled", "font", 
  "fontSize", "fontStyle", "fontWeight", "href", "interpolate", "limit", "line", 
  "opacity", "order", "orient", "point", "radius", "shape", "size", "stroke", 
  "strokeCap", "strokeDash", "strokeDashOffset", "strokeJoin", "strokeMiterLimit", 
  "strokeOpacity", "strokeWidth", "style", "tension", "text", "theta", "thickness", 
  "tooltip", "type", "x", "x2", "x2Offset", "xOffset", "y", "y2", "y2Offset", 
  "yOffset"))
  .add_mark(args$spec, args$object, '#/definitions/MarkDef', args$extra)
} #' vl_mark_rect
#'
#'
#' @param spec An input vega-lite spec
#' @param align (_MarkDef_) The horizontal alignment of the text. One of `"left"`, `"right"`, `"center"`.
#' @param angle (_MarkDef_) The rotation angle of the text, in degrees.
#' @param baseline (_MarkDef_) The vertical alignment of the text. One of `"top"`, `"middle"`, `"bottom"`.
#' 
#' __Default value:__ `"middle"`
#' @param binSpacing (_MarkDef_) Offset between bars for binned field.  Ideal value for this is either 0 (Preferred by statisticians) or 1 (Vega-Lite Default, D3 example style).
#' 
#' __Default value:__ `1`
#' @param clip (_MarkDef_) Whether a mark be clipped to the enclosing group’s width and height.
#' @param color (_MarkDef_) Default color.  Note that `fill` and `stroke` have higher precedence than `color` and will override `color`.
#' 
#' __Default value:__ <span style="color: #4682b4;">&#9632;</span> `"#4682b4"`
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param cornerRadius (_MarkDef_) The radius in pixels of rounded rectangle corners.
#' 
#' __Default value:__ `0`
#' @param cursor (_MarkDef_) The mouse cursor used over the mark. Any valid [CSS cursor type](https://developer.mozilla.org/en-US/docs/Web/CSS/cursor#Values) can be used.
#' @param dir (_MarkDef_) The direction of the text. One of `"ltr"` (left-to-right) or `"rtl"` (right-to-left). This property determines on which side is truncated in response to the limit parameter.
#' 
#' __Default value:__ `"ltr"`
#' @param dx (_MarkDef_) The horizontal offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param dy (_MarkDef_) The vertical offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param ellipsis (_MarkDef_) The ellipsis string for text truncated in response to the limit parameter.
#' 
#' __Default value:__ `"…"`
#' @param fill (_MarkDef_) Default Fill Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param fillOpacity (_MarkDef_) The fill opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param filled (_MarkDef_) Whether the mark's color should be used as fill color instead of stroke color.
#' 
#' __Default value:__ `false` for `point`, `line` and `rule`; otherwise, `true`.
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param font (_MarkDef_) The typeface to set the text in (e.g., `"Helvetica Neue"`).
#' @param fontSize (_MarkDef_) The font size, in pixels.
#' @param fontStyle (_MarkDef_) The font style (e.g., `"italic"`).
#' @param fontWeight (_MarkDef_) The font weight.
#' This can be either a string (e.g `"bold"`, `"normal"`) or a number (`100`, `200`, `300`, ..., `900` where `"normal"` = `400` and `"bold"` = `700`).
#' @param href (_MarkDef_) A URL to load upon mouse click. If defined, the mark acts as a hyperlink.
#' @param interpolate (_MarkDef_) The line interpolation method to use for line and area marks. One of the following:
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
#' - `"monotone"`: cubic interpolation that preserves monotonicity in y.
#' @param limit (_MarkDef_) The maximum length of the text mark in pixels. The text value will be automatically truncated if the rendered size exceeds the limit.
#' 
#' __Default value:__ `0`, indicating no limit
#' @param line (_MarkDef_) A flag for overlaying line on top of area marks, or an object defining the properties of the overlayed lines.
#' 
#' - If this value is an empty object (`{}`) or `true`, lines with default properties will be used.
#' 
#' - If this value is `false`, no lines would be automatically added to area marks.
#' 
#' __Default value:__ `false`.
#' @param opacity (_MarkDef_) The overall opacity (value between \[0,1\]).
#' 
#' __Default value:__ `0.7` for non-aggregate plots with `point`, `tick`, `circle`, or `square` marks or layered `bar` charts and `1` otherwise.
#' @param order (_MarkDef_) For line and trail marks, this `order` property can be set to `null` or `false` to make the lines use the original order in the data sources.
#' @param orient (_MarkDef_) The orientation of a non-stacked bar, tick, area, and line charts.
#' The value is either horizontal (default) or vertical.
#' - For bar, rule and tick, this determines whether the size of the bar and tick
#' should be applied to x or y dimension.
#' - For area, this property determines the orient property of the Vega output.
#' - For line and trail marks, this property determines the sort order of the points in the line
#' if `config.sortLineBy` is not specified.
#' For stacked charts, this is always determined by the orientation of the stack;
#' therefore explicitly specified value will be ignored.
#' @param point (_MarkDef_) A flag for overlaying points on top of line or area marks, or an object defining the properties of the overlayed points.
#' 
#' - If this property is `"transparent"`, transparent points will be used (for enhancing tooltips and selections).
#' 
#' - If this property is an empty object (`{}`) or `true`, filled points with default properties will be used.
#' 
#' - If this property is `false`, no points would be automatically added to line or area marks.
#' 
#' __Default value:__ `false`.
#' @param radius (_MarkDef_) Polar coordinate radial offset, in pixels, of the text label from the origin determined by the `x` and `y` properties.
#' @param shape (_MarkDef_) Shape of the point marks. Supported values include:
#' - plotting shapes: `"circle"`, `"square"`, `"cross"`, `"diamond"`, `"triangle-up"`, `"triangle-down"`, `"triangle-right"`, or `"triangle-left"`.
#' - the line symbol `"stroke"`
#' - centered directional shapes `"arrow"`, `"wedge"`, or `"triangle"`
#' - a custom [SVG path string](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) (For correct sizing, custom shape paths should be defined within a square bounding box with coordinates ranging from -1 to 1 along both the x and y dimensions.)
#' 
#' __Default value:__ `"circle"`
#' @param size (_MarkDef_) Default size for marks.
#' - For `point`/`circle`/`square`, this represents the pixel area of the marks. For example: in the case of circles, the radius is determined in part by the square root of the size value.
#' - For `bar`, this represents the band size of the bar, in pixels.
#' - For `text`, this represents the font size, in pixels.
#' 
#' __Default value:__ `30` for point, circle, square marks; `rangeStep` - 1 for bar marks with discrete dimensions; `5` for bar marks with continuous dimensions; `11` for text marks.
#' @param stroke (_MarkDef_) Default Stroke Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param strokeCap (_MarkDef_) The stroke cap for line ending style. One of `"butt"`, `"round"`, or `"square"`.
#' 
#' __Default value:__ `"square"`
#' @param strokeDash (_MarkDef_) An array of alternating stroke, space lengths for creating dashed or dotted lines.
#' @param strokeDashOffset (_MarkDef_) The offset (in pixels) into which to begin drawing with the stroke dash array.
#' @param strokeJoin (_MarkDef_) The stroke line join method. One of `"miter"`, `"round"` or `"bevel"`.
#' 
#' __Default value:__ `"miter"`
#' @param strokeMiterLimit (_MarkDef_) The miter limit at which to bevel a line join.
#' @param strokeOpacity (_MarkDef_) The stroke opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param strokeWidth (_MarkDef_) The stroke width, in pixels.
#' @param style (_MarkDef_) A string or array of strings indicating the name of custom styles to apply to the mark. A style is a named collection of mark property defaults defined within the [style configuration](https://vega.github.io/vega-lite/docs/mark.html#style-config). If style is an array, later styles will override earlier styles. Any [mark properties](https://vega.github.io/vega-lite/docs/encoding.html#mark-prop) explicitly defined within the `encoding` will override a style default.
#' 
#' __Default value:__ The mark's name.  For example, a bar mark will have style `"bar"` by default.
#' __Note:__ Any specified style will augment the default style. For example, a bar mark with `"style": "foo"` will receive from `config.style.bar` and `config.style.foo` (the specified style `"foo"` has higher precedence).
#' @param tension (_MarkDef_) Depending on the interpolation type, sets the tension parameter (for line and area marks).
#' @param text (_MarkDef_) Placeholder text if the `text` channel is not specified
#' @param theta (_MarkDef_) Polar coordinate angle, in radians, of the text label from the origin determined by the `x` and `y` properties. Values for `theta` follow the same convention of `arc` mark `startAngle` and `endAngle` properties: angles are measured in radians, with `0` indicating "north".
#' @param thickness (_MarkDef_) Thickness of the tick mark.
#' 
#' __Default value:__  `1`
#' @param tooltip (_MarkDef_) The tooltip text string to show upon mouse hover or an object defining which fields should the tooltip be derived from.
#' 
#' - If `tooltip` is `{"content": "encoding"}`, then all fields from `encoding` will be used.
#' - If `tooltip` is `{"content": "data"}`, then all fields that appear in the highlighted data point will be used.
#' - If set to `null`, then no tooltip will be used.
#' @param x (_MarkDef_) X coordinates of the marks, or width of horizontal `"bar"` and `"area"` without `x2`.
#' @param x2 (_MarkDef_) X2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param x2Offset (_MarkDef_) Offset for x2-position.
#' @param xOffset (_MarkDef_) Offset for x-position.
#' @param y (_MarkDef_) Y coordinates of the marks, or height of vertical `"bar"` and `"area"` without `y2`
#' @param y2 (_MarkDef_) Y2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param y2Offset (_MarkDef_) Offset for y2-position.
#' @param yOffset (_MarkDef_) Offset for y-position.
#' @return A modified Vega-Lite Spec
#' @export
vl_mark_rect <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL){
  args <- .modify_args(list(type = "rect"), c("align", "angle", "baseline", "binSpacing", "clip", "color", "cornerRadius", 
  "cursor", "dir", "dx", "dy", "ellipsis", "fill", "fillOpacity", "filled", "font", 
  "fontSize", "fontStyle", "fontWeight", "href", "interpolate", "limit", "line", 
  "opacity", "order", "orient", "point", "radius", "shape", "size", "stroke", 
  "strokeCap", "strokeDash", "strokeDashOffset", "strokeJoin", "strokeMiterLimit", 
  "strokeOpacity", "strokeWidth", "style", "tension", "text", "theta", "thickness", 
  "tooltip", "type", "x", "x2", "x2Offset", "xOffset", "y", "y2", "y2Offset", 
  "yOffset"))
  .add_mark(args$spec, args$object, '#/definitions/MarkDef', args$extra)
} #' vl_mark_rule
#'
#'
#' @param spec An input vega-lite spec
#' @param align (_MarkDef_) The horizontal alignment of the text. One of `"left"`, `"right"`, `"center"`.
#' @param angle (_MarkDef_) The rotation angle of the text, in degrees.
#' @param baseline (_MarkDef_) The vertical alignment of the text. One of `"top"`, `"middle"`, `"bottom"`.
#' 
#' __Default value:__ `"middle"`
#' @param binSpacing (_MarkDef_) Offset between bars for binned field.  Ideal value for this is either 0 (Preferred by statisticians) or 1 (Vega-Lite Default, D3 example style).
#' 
#' __Default value:__ `1`
#' @param clip (_MarkDef_) Whether a mark be clipped to the enclosing group’s width and height.
#' @param color (_MarkDef_) Default color.  Note that `fill` and `stroke` have higher precedence than `color` and will override `color`.
#' 
#' __Default value:__ <span style="color: #4682b4;">&#9632;</span> `"#4682b4"`
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param cornerRadius (_MarkDef_) The radius in pixels of rounded rectangle corners.
#' 
#' __Default value:__ `0`
#' @param cursor (_MarkDef_) The mouse cursor used over the mark. Any valid [CSS cursor type](https://developer.mozilla.org/en-US/docs/Web/CSS/cursor#Values) can be used.
#' @param dir (_MarkDef_) The direction of the text. One of `"ltr"` (left-to-right) or `"rtl"` (right-to-left). This property determines on which side is truncated in response to the limit parameter.
#' 
#' __Default value:__ `"ltr"`
#' @param dx (_MarkDef_) The horizontal offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param dy (_MarkDef_) The vertical offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param ellipsis (_MarkDef_) The ellipsis string for text truncated in response to the limit parameter.
#' 
#' __Default value:__ `"…"`
#' @param fill (_MarkDef_) Default Fill Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param fillOpacity (_MarkDef_) The fill opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param filled (_MarkDef_) Whether the mark's color should be used as fill color instead of stroke color.
#' 
#' __Default value:__ `false` for `point`, `line` and `rule`; otherwise, `true`.
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param font (_MarkDef_) The typeface to set the text in (e.g., `"Helvetica Neue"`).
#' @param fontSize (_MarkDef_) The font size, in pixels.
#' @param fontStyle (_MarkDef_) The font style (e.g., `"italic"`).
#' @param fontWeight (_MarkDef_) The font weight.
#' This can be either a string (e.g `"bold"`, `"normal"`) or a number (`100`, `200`, `300`, ..., `900` where `"normal"` = `400` and `"bold"` = `700`).
#' @param href (_MarkDef_) A URL to load upon mouse click. If defined, the mark acts as a hyperlink.
#' @param interpolate (_MarkDef_) The line interpolation method to use for line and area marks. One of the following:
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
#' - `"monotone"`: cubic interpolation that preserves monotonicity in y.
#' @param limit (_MarkDef_) The maximum length of the text mark in pixels. The text value will be automatically truncated if the rendered size exceeds the limit.
#' 
#' __Default value:__ `0`, indicating no limit
#' @param line (_MarkDef_) A flag for overlaying line on top of area marks, or an object defining the properties of the overlayed lines.
#' 
#' - If this value is an empty object (`{}`) or `true`, lines with default properties will be used.
#' 
#' - If this value is `false`, no lines would be automatically added to area marks.
#' 
#' __Default value:__ `false`.
#' @param opacity (_MarkDef_) The overall opacity (value between \[0,1\]).
#' 
#' __Default value:__ `0.7` for non-aggregate plots with `point`, `tick`, `circle`, or `square` marks or layered `bar` charts and `1` otherwise.
#' @param order (_MarkDef_) For line and trail marks, this `order` property can be set to `null` or `false` to make the lines use the original order in the data sources.
#' @param orient (_MarkDef_) The orientation of a non-stacked bar, tick, area, and line charts.
#' The value is either horizontal (default) or vertical.
#' - For bar, rule and tick, this determines whether the size of the bar and tick
#' should be applied to x or y dimension.
#' - For area, this property determines the orient property of the Vega output.
#' - For line and trail marks, this property determines the sort order of the points in the line
#' if `config.sortLineBy` is not specified.
#' For stacked charts, this is always determined by the orientation of the stack;
#' therefore explicitly specified value will be ignored.
#' @param point (_MarkDef_) A flag for overlaying points on top of line or area marks, or an object defining the properties of the overlayed points.
#' 
#' - If this property is `"transparent"`, transparent points will be used (for enhancing tooltips and selections).
#' 
#' - If this property is an empty object (`{}`) or `true`, filled points with default properties will be used.
#' 
#' - If this property is `false`, no points would be automatically added to line or area marks.
#' 
#' __Default value:__ `false`.
#' @param radius (_MarkDef_) Polar coordinate radial offset, in pixels, of the text label from the origin determined by the `x` and `y` properties.
#' @param shape (_MarkDef_) Shape of the point marks. Supported values include:
#' - plotting shapes: `"circle"`, `"square"`, `"cross"`, `"diamond"`, `"triangle-up"`, `"triangle-down"`, `"triangle-right"`, or `"triangle-left"`.
#' - the line symbol `"stroke"`
#' - centered directional shapes `"arrow"`, `"wedge"`, or `"triangle"`
#' - a custom [SVG path string](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) (For correct sizing, custom shape paths should be defined within a square bounding box with coordinates ranging from -1 to 1 along both the x and y dimensions.)
#' 
#' __Default value:__ `"circle"`
#' @param size (_MarkDef_) Default size for marks.
#' - For `point`/`circle`/`square`, this represents the pixel area of the marks. For example: in the case of circles, the radius is determined in part by the square root of the size value.
#' - For `bar`, this represents the band size of the bar, in pixels.
#' - For `text`, this represents the font size, in pixels.
#' 
#' __Default value:__ `30` for point, circle, square marks; `rangeStep` - 1 for bar marks with discrete dimensions; `5` for bar marks with continuous dimensions; `11` for text marks.
#' @param stroke (_MarkDef_) Default Stroke Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param strokeCap (_MarkDef_) The stroke cap for line ending style. One of `"butt"`, `"round"`, or `"square"`.
#' 
#' __Default value:__ `"square"`
#' @param strokeDash (_MarkDef_) An array of alternating stroke, space lengths for creating dashed or dotted lines.
#' @param strokeDashOffset (_MarkDef_) The offset (in pixels) into which to begin drawing with the stroke dash array.
#' @param strokeJoin (_MarkDef_) The stroke line join method. One of `"miter"`, `"round"` or `"bevel"`.
#' 
#' __Default value:__ `"miter"`
#' @param strokeMiterLimit (_MarkDef_) The miter limit at which to bevel a line join.
#' @param strokeOpacity (_MarkDef_) The stroke opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param strokeWidth (_MarkDef_) The stroke width, in pixels.
#' @param style (_MarkDef_) A string or array of strings indicating the name of custom styles to apply to the mark. A style is a named collection of mark property defaults defined within the [style configuration](https://vega.github.io/vega-lite/docs/mark.html#style-config). If style is an array, later styles will override earlier styles. Any [mark properties](https://vega.github.io/vega-lite/docs/encoding.html#mark-prop) explicitly defined within the `encoding` will override a style default.
#' 
#' __Default value:__ The mark's name.  For example, a bar mark will have style `"bar"` by default.
#' __Note:__ Any specified style will augment the default style. For example, a bar mark with `"style": "foo"` will receive from `config.style.bar` and `config.style.foo` (the specified style `"foo"` has higher precedence).
#' @param tension (_MarkDef_) Depending on the interpolation type, sets the tension parameter (for line and area marks).
#' @param text (_MarkDef_) Placeholder text if the `text` channel is not specified
#' @param theta (_MarkDef_) Polar coordinate angle, in radians, of the text label from the origin determined by the `x` and `y` properties. Values for `theta` follow the same convention of `arc` mark `startAngle` and `endAngle` properties: angles are measured in radians, with `0` indicating "north".
#' @param thickness (_MarkDef_) Thickness of the tick mark.
#' 
#' __Default value:__  `1`
#' @param tooltip (_MarkDef_) The tooltip text string to show upon mouse hover or an object defining which fields should the tooltip be derived from.
#' 
#' - If `tooltip` is `{"content": "encoding"}`, then all fields from `encoding` will be used.
#' - If `tooltip` is `{"content": "data"}`, then all fields that appear in the highlighted data point will be used.
#' - If set to `null`, then no tooltip will be used.
#' @param x (_MarkDef_) X coordinates of the marks, or width of horizontal `"bar"` and `"area"` without `x2`.
#' @param x2 (_MarkDef_) X2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param x2Offset (_MarkDef_) Offset for x2-position.
#' @param xOffset (_MarkDef_) Offset for x-position.
#' @param y (_MarkDef_) Y coordinates of the marks, or height of vertical `"bar"` and `"area"` without `y2`
#' @param y2 (_MarkDef_) Y2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param y2Offset (_MarkDef_) Offset for y2-position.
#' @param yOffset (_MarkDef_) Offset for y-position.
#' @return A modified Vega-Lite Spec
#' @export
vl_mark_rule <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL){
  args <- .modify_args(list(type = "rule"), c("align", "angle", "baseline", "binSpacing", "clip", "color", "cornerRadius", 
  "cursor", "dir", "dx", "dy", "ellipsis", "fill", "fillOpacity", "filled", "font", 
  "fontSize", "fontStyle", "fontWeight", "href", "interpolate", "limit", "line", 
  "opacity", "order", "orient", "point", "radius", "shape", "size", "stroke", 
  "strokeCap", "strokeDash", "strokeDashOffset", "strokeJoin", "strokeMiterLimit", 
  "strokeOpacity", "strokeWidth", "style", "tension", "text", "theta", "thickness", 
  "tooltip", "type", "x", "x2", "x2Offset", "xOffset", "y", "y2", "y2Offset", 
  "yOffset"))
  .add_mark(args$spec, args$object, '#/definitions/MarkDef', args$extra)
} #' vl_mark_circle
#'
#'
#' @param spec An input vega-lite spec
#' @param align (_MarkDef_) The horizontal alignment of the text. One of `"left"`, `"right"`, `"center"`.
#' @param angle (_MarkDef_) The rotation angle of the text, in degrees.
#' @param baseline (_MarkDef_) The vertical alignment of the text. One of `"top"`, `"middle"`, `"bottom"`.
#' 
#' __Default value:__ `"middle"`
#' @param binSpacing (_MarkDef_) Offset between bars for binned field.  Ideal value for this is either 0 (Preferred by statisticians) or 1 (Vega-Lite Default, D3 example style).
#' 
#' __Default value:__ `1`
#' @param clip (_MarkDef_) Whether a mark be clipped to the enclosing group’s width and height.
#' @param color (_MarkDef_) Default color.  Note that `fill` and `stroke` have higher precedence than `color` and will override `color`.
#' 
#' __Default value:__ <span style="color: #4682b4;">&#9632;</span> `"#4682b4"`
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param cornerRadius (_MarkDef_) The radius in pixels of rounded rectangle corners.
#' 
#' __Default value:__ `0`
#' @param cursor (_MarkDef_) The mouse cursor used over the mark. Any valid [CSS cursor type](https://developer.mozilla.org/en-US/docs/Web/CSS/cursor#Values) can be used.
#' @param dir (_MarkDef_) The direction of the text. One of `"ltr"` (left-to-right) or `"rtl"` (right-to-left). This property determines on which side is truncated in response to the limit parameter.
#' 
#' __Default value:__ `"ltr"`
#' @param dx (_MarkDef_) The horizontal offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param dy (_MarkDef_) The vertical offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param ellipsis (_MarkDef_) The ellipsis string for text truncated in response to the limit parameter.
#' 
#' __Default value:__ `"…"`
#' @param fill (_MarkDef_) Default Fill Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param fillOpacity (_MarkDef_) The fill opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param filled (_MarkDef_) Whether the mark's color should be used as fill color instead of stroke color.
#' 
#' __Default value:__ `false` for `point`, `line` and `rule`; otherwise, `true`.
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param font (_MarkDef_) The typeface to set the text in (e.g., `"Helvetica Neue"`).
#' @param fontSize (_MarkDef_) The font size, in pixels.
#' @param fontStyle (_MarkDef_) The font style (e.g., `"italic"`).
#' @param fontWeight (_MarkDef_) The font weight.
#' This can be either a string (e.g `"bold"`, `"normal"`) or a number (`100`, `200`, `300`, ..., `900` where `"normal"` = `400` and `"bold"` = `700`).
#' @param href (_MarkDef_) A URL to load upon mouse click. If defined, the mark acts as a hyperlink.
#' @param interpolate (_MarkDef_) The line interpolation method to use for line and area marks. One of the following:
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
#' - `"monotone"`: cubic interpolation that preserves monotonicity in y.
#' @param limit (_MarkDef_) The maximum length of the text mark in pixels. The text value will be automatically truncated if the rendered size exceeds the limit.
#' 
#' __Default value:__ `0`, indicating no limit
#' @param line (_MarkDef_) A flag for overlaying line on top of area marks, or an object defining the properties of the overlayed lines.
#' 
#' - If this value is an empty object (`{}`) or `true`, lines with default properties will be used.
#' 
#' - If this value is `false`, no lines would be automatically added to area marks.
#' 
#' __Default value:__ `false`.
#' @param opacity (_MarkDef_) The overall opacity (value between \[0,1\]).
#' 
#' __Default value:__ `0.7` for non-aggregate plots with `point`, `tick`, `circle`, or `square` marks or layered `bar` charts and `1` otherwise.
#' @param order (_MarkDef_) For line and trail marks, this `order` property can be set to `null` or `false` to make the lines use the original order in the data sources.
#' @param orient (_MarkDef_) The orientation of a non-stacked bar, tick, area, and line charts.
#' The value is either horizontal (default) or vertical.
#' - For bar, rule and tick, this determines whether the size of the bar and tick
#' should be applied to x or y dimension.
#' - For area, this property determines the orient property of the Vega output.
#' - For line and trail marks, this property determines the sort order of the points in the line
#' if `config.sortLineBy` is not specified.
#' For stacked charts, this is always determined by the orientation of the stack;
#' therefore explicitly specified value will be ignored.
#' @param point (_MarkDef_) A flag for overlaying points on top of line or area marks, or an object defining the properties of the overlayed points.
#' 
#' - If this property is `"transparent"`, transparent points will be used (for enhancing tooltips and selections).
#' 
#' - If this property is an empty object (`{}`) or `true`, filled points with default properties will be used.
#' 
#' - If this property is `false`, no points would be automatically added to line or area marks.
#' 
#' __Default value:__ `false`.
#' @param radius (_MarkDef_) Polar coordinate radial offset, in pixels, of the text label from the origin determined by the `x` and `y` properties.
#' @param shape (_MarkDef_) Shape of the point marks. Supported values include:
#' - plotting shapes: `"circle"`, `"square"`, `"cross"`, `"diamond"`, `"triangle-up"`, `"triangle-down"`, `"triangle-right"`, or `"triangle-left"`.
#' - the line symbol `"stroke"`
#' - centered directional shapes `"arrow"`, `"wedge"`, or `"triangle"`
#' - a custom [SVG path string](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) (For correct sizing, custom shape paths should be defined within a square bounding box with coordinates ranging from -1 to 1 along both the x and y dimensions.)
#' 
#' __Default value:__ `"circle"`
#' @param size (_MarkDef_) Default size for marks.
#' - For `point`/`circle`/`square`, this represents the pixel area of the marks. For example: in the case of circles, the radius is determined in part by the square root of the size value.
#' - For `bar`, this represents the band size of the bar, in pixels.
#' - For `text`, this represents the font size, in pixels.
#' 
#' __Default value:__ `30` for point, circle, square marks; `rangeStep` - 1 for bar marks with discrete dimensions; `5` for bar marks with continuous dimensions; `11` for text marks.
#' @param stroke (_MarkDef_) Default Stroke Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param strokeCap (_MarkDef_) The stroke cap for line ending style. One of `"butt"`, `"round"`, or `"square"`.
#' 
#' __Default value:__ `"square"`
#' @param strokeDash (_MarkDef_) An array of alternating stroke, space lengths for creating dashed or dotted lines.
#' @param strokeDashOffset (_MarkDef_) The offset (in pixels) into which to begin drawing with the stroke dash array.
#' @param strokeJoin (_MarkDef_) The stroke line join method. One of `"miter"`, `"round"` or `"bevel"`.
#' 
#' __Default value:__ `"miter"`
#' @param strokeMiterLimit (_MarkDef_) The miter limit at which to bevel a line join.
#' @param strokeOpacity (_MarkDef_) The stroke opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param strokeWidth (_MarkDef_) The stroke width, in pixels.
#' @param style (_MarkDef_) A string or array of strings indicating the name of custom styles to apply to the mark. A style is a named collection of mark property defaults defined within the [style configuration](https://vega.github.io/vega-lite/docs/mark.html#style-config). If style is an array, later styles will override earlier styles. Any [mark properties](https://vega.github.io/vega-lite/docs/encoding.html#mark-prop) explicitly defined within the `encoding` will override a style default.
#' 
#' __Default value:__ The mark's name.  For example, a bar mark will have style `"bar"` by default.
#' __Note:__ Any specified style will augment the default style. For example, a bar mark with `"style": "foo"` will receive from `config.style.bar` and `config.style.foo` (the specified style `"foo"` has higher precedence).
#' @param tension (_MarkDef_) Depending on the interpolation type, sets the tension parameter (for line and area marks).
#' @param text (_MarkDef_) Placeholder text if the `text` channel is not specified
#' @param theta (_MarkDef_) Polar coordinate angle, in radians, of the text label from the origin determined by the `x` and `y` properties. Values for `theta` follow the same convention of `arc` mark `startAngle` and `endAngle` properties: angles are measured in radians, with `0` indicating "north".
#' @param thickness (_MarkDef_) Thickness of the tick mark.
#' 
#' __Default value:__  `1`
#' @param tooltip (_MarkDef_) The tooltip text string to show upon mouse hover or an object defining which fields should the tooltip be derived from.
#' 
#' - If `tooltip` is `{"content": "encoding"}`, then all fields from `encoding` will be used.
#' - If `tooltip` is `{"content": "data"}`, then all fields that appear in the highlighted data point will be used.
#' - If set to `null`, then no tooltip will be used.
#' @param x (_MarkDef_) X coordinates of the marks, or width of horizontal `"bar"` and `"area"` without `x2`.
#' @param x2 (_MarkDef_) X2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param x2Offset (_MarkDef_) Offset for x2-position.
#' @param xOffset (_MarkDef_) Offset for x-position.
#' @param y (_MarkDef_) Y coordinates of the marks, or height of vertical `"bar"` and `"area"` without `y2`
#' @param y2 (_MarkDef_) Y2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param y2Offset (_MarkDef_) Offset for y2-position.
#' @param yOffset (_MarkDef_) Offset for y-position.
#' @return A modified Vega-Lite Spec
#' @export
vl_mark_circle <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL){
  args <- .modify_args(list(type = "circle"), c("align", "angle", "baseline", "binSpacing", "clip", "color", "cornerRadius", 
  "cursor", "dir", "dx", "dy", "ellipsis", "fill", "fillOpacity", "filled", "font", 
  "fontSize", "fontStyle", "fontWeight", "href", "interpolate", "limit", "line", 
  "opacity", "order", "orient", "point", "radius", "shape", "size", "stroke", 
  "strokeCap", "strokeDash", "strokeDashOffset", "strokeJoin", "strokeMiterLimit", 
  "strokeOpacity", "strokeWidth", "style", "tension", "text", "theta", "thickness", 
  "tooltip", "type", "x", "x2", "x2Offset", "xOffset", "y", "y2", "y2Offset", 
  "yOffset"))
  .add_mark(args$spec, args$object, '#/definitions/MarkDef', args$extra)
} #' vl_mark_square
#'
#'
#' @param spec An input vega-lite spec
#' @param align (_MarkDef_) The horizontal alignment of the text. One of `"left"`, `"right"`, `"center"`.
#' @param angle (_MarkDef_) The rotation angle of the text, in degrees.
#' @param baseline (_MarkDef_) The vertical alignment of the text. One of `"top"`, `"middle"`, `"bottom"`.
#' 
#' __Default value:__ `"middle"`
#' @param binSpacing (_MarkDef_) Offset between bars for binned field.  Ideal value for this is either 0 (Preferred by statisticians) or 1 (Vega-Lite Default, D3 example style).
#' 
#' __Default value:__ `1`
#' @param clip (_MarkDef_) Whether a mark be clipped to the enclosing group’s width and height.
#' @param color (_MarkDef_) Default color.  Note that `fill` and `stroke` have higher precedence than `color` and will override `color`.
#' 
#' __Default value:__ <span style="color: #4682b4;">&#9632;</span> `"#4682b4"`
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param cornerRadius (_MarkDef_) The radius in pixels of rounded rectangle corners.
#' 
#' __Default value:__ `0`
#' @param cursor (_MarkDef_) The mouse cursor used over the mark. Any valid [CSS cursor type](https://developer.mozilla.org/en-US/docs/Web/CSS/cursor#Values) can be used.
#' @param dir (_MarkDef_) The direction of the text. One of `"ltr"` (left-to-right) or `"rtl"` (right-to-left). This property determines on which side is truncated in response to the limit parameter.
#' 
#' __Default value:__ `"ltr"`
#' @param dx (_MarkDef_) The horizontal offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param dy (_MarkDef_) The vertical offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param ellipsis (_MarkDef_) The ellipsis string for text truncated in response to the limit parameter.
#' 
#' __Default value:__ `"…"`
#' @param fill (_MarkDef_) Default Fill Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param fillOpacity (_MarkDef_) The fill opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param filled (_MarkDef_) Whether the mark's color should be used as fill color instead of stroke color.
#' 
#' __Default value:__ `false` for `point`, `line` and `rule`; otherwise, `true`.
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param font (_MarkDef_) The typeface to set the text in (e.g., `"Helvetica Neue"`).
#' @param fontSize (_MarkDef_) The font size, in pixels.
#' @param fontStyle (_MarkDef_) The font style (e.g., `"italic"`).
#' @param fontWeight (_MarkDef_) The font weight.
#' This can be either a string (e.g `"bold"`, `"normal"`) or a number (`100`, `200`, `300`, ..., `900` where `"normal"` = `400` and `"bold"` = `700`).
#' @param href (_MarkDef_) A URL to load upon mouse click. If defined, the mark acts as a hyperlink.
#' @param interpolate (_MarkDef_) The line interpolation method to use for line and area marks. One of the following:
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
#' - `"monotone"`: cubic interpolation that preserves monotonicity in y.
#' @param limit (_MarkDef_) The maximum length of the text mark in pixels. The text value will be automatically truncated if the rendered size exceeds the limit.
#' 
#' __Default value:__ `0`, indicating no limit
#' @param line (_MarkDef_) A flag for overlaying line on top of area marks, or an object defining the properties of the overlayed lines.
#' 
#' - If this value is an empty object (`{}`) or `true`, lines with default properties will be used.
#' 
#' - If this value is `false`, no lines would be automatically added to area marks.
#' 
#' __Default value:__ `false`.
#' @param opacity (_MarkDef_) The overall opacity (value between \[0,1\]).
#' 
#' __Default value:__ `0.7` for non-aggregate plots with `point`, `tick`, `circle`, or `square` marks or layered `bar` charts and `1` otherwise.
#' @param order (_MarkDef_) For line and trail marks, this `order` property can be set to `null` or `false` to make the lines use the original order in the data sources.
#' @param orient (_MarkDef_) The orientation of a non-stacked bar, tick, area, and line charts.
#' The value is either horizontal (default) or vertical.
#' - For bar, rule and tick, this determines whether the size of the bar and tick
#' should be applied to x or y dimension.
#' - For area, this property determines the orient property of the Vega output.
#' - For line and trail marks, this property determines the sort order of the points in the line
#' if `config.sortLineBy` is not specified.
#' For stacked charts, this is always determined by the orientation of the stack;
#' therefore explicitly specified value will be ignored.
#' @param point (_MarkDef_) A flag for overlaying points on top of line or area marks, or an object defining the properties of the overlayed points.
#' 
#' - If this property is `"transparent"`, transparent points will be used (for enhancing tooltips and selections).
#' 
#' - If this property is an empty object (`{}`) or `true`, filled points with default properties will be used.
#' 
#' - If this property is `false`, no points would be automatically added to line or area marks.
#' 
#' __Default value:__ `false`.
#' @param radius (_MarkDef_) Polar coordinate radial offset, in pixels, of the text label from the origin determined by the `x` and `y` properties.
#' @param shape (_MarkDef_) Shape of the point marks. Supported values include:
#' - plotting shapes: `"circle"`, `"square"`, `"cross"`, `"diamond"`, `"triangle-up"`, `"triangle-down"`, `"triangle-right"`, or `"triangle-left"`.
#' - the line symbol `"stroke"`
#' - centered directional shapes `"arrow"`, `"wedge"`, or `"triangle"`
#' - a custom [SVG path string](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) (For correct sizing, custom shape paths should be defined within a square bounding box with coordinates ranging from -1 to 1 along both the x and y dimensions.)
#' 
#' __Default value:__ `"circle"`
#' @param size (_MarkDef_) Default size for marks.
#' - For `point`/`circle`/`square`, this represents the pixel area of the marks. For example: in the case of circles, the radius is determined in part by the square root of the size value.
#' - For `bar`, this represents the band size of the bar, in pixels.
#' - For `text`, this represents the font size, in pixels.
#' 
#' __Default value:__ `30` for point, circle, square marks; `rangeStep` - 1 for bar marks with discrete dimensions; `5` for bar marks with continuous dimensions; `11` for text marks.
#' @param stroke (_MarkDef_) Default Stroke Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param strokeCap (_MarkDef_) The stroke cap for line ending style. One of `"butt"`, `"round"`, or `"square"`.
#' 
#' __Default value:__ `"square"`
#' @param strokeDash (_MarkDef_) An array of alternating stroke, space lengths for creating dashed or dotted lines.
#' @param strokeDashOffset (_MarkDef_) The offset (in pixels) into which to begin drawing with the stroke dash array.
#' @param strokeJoin (_MarkDef_) The stroke line join method. One of `"miter"`, `"round"` or `"bevel"`.
#' 
#' __Default value:__ `"miter"`
#' @param strokeMiterLimit (_MarkDef_) The miter limit at which to bevel a line join.
#' @param strokeOpacity (_MarkDef_) The stroke opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param strokeWidth (_MarkDef_) The stroke width, in pixels.
#' @param style (_MarkDef_) A string or array of strings indicating the name of custom styles to apply to the mark. A style is a named collection of mark property defaults defined within the [style configuration](https://vega.github.io/vega-lite/docs/mark.html#style-config). If style is an array, later styles will override earlier styles. Any [mark properties](https://vega.github.io/vega-lite/docs/encoding.html#mark-prop) explicitly defined within the `encoding` will override a style default.
#' 
#' __Default value:__ The mark's name.  For example, a bar mark will have style `"bar"` by default.
#' __Note:__ Any specified style will augment the default style. For example, a bar mark with `"style": "foo"` will receive from `config.style.bar` and `config.style.foo` (the specified style `"foo"` has higher precedence).
#' @param tension (_MarkDef_) Depending on the interpolation type, sets the tension parameter (for line and area marks).
#' @param text (_MarkDef_) Placeholder text if the `text` channel is not specified
#' @param theta (_MarkDef_) Polar coordinate angle, in radians, of the text label from the origin determined by the `x` and `y` properties. Values for `theta` follow the same convention of `arc` mark `startAngle` and `endAngle` properties: angles are measured in radians, with `0` indicating "north".
#' @param thickness (_MarkDef_) Thickness of the tick mark.
#' 
#' __Default value:__  `1`
#' @param tooltip (_MarkDef_) The tooltip text string to show upon mouse hover or an object defining which fields should the tooltip be derived from.
#' 
#' - If `tooltip` is `{"content": "encoding"}`, then all fields from `encoding` will be used.
#' - If `tooltip` is `{"content": "data"}`, then all fields that appear in the highlighted data point will be used.
#' - If set to `null`, then no tooltip will be used.
#' @param x (_MarkDef_) X coordinates of the marks, or width of horizontal `"bar"` and `"area"` without `x2`.
#' @param x2 (_MarkDef_) X2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param x2Offset (_MarkDef_) Offset for x2-position.
#' @param xOffset (_MarkDef_) Offset for x-position.
#' @param y (_MarkDef_) Y coordinates of the marks, or height of vertical `"bar"` and `"area"` without `y2`
#' @param y2 (_MarkDef_) Y2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param y2Offset (_MarkDef_) Offset for y2-position.
#' @param yOffset (_MarkDef_) Offset for y-position.
#' @return A modified Vega-Lite Spec
#' @export
vl_mark_square <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL){
  args <- .modify_args(list(type = "square"), c("align", "angle", "baseline", "binSpacing", "clip", "color", "cornerRadius", 
  "cursor", "dir", "dx", "dy", "ellipsis", "fill", "fillOpacity", "filled", "font", 
  "fontSize", "fontStyle", "fontWeight", "href", "interpolate", "limit", "line", 
  "opacity", "order", "orient", "point", "radius", "shape", "size", "stroke", 
  "strokeCap", "strokeDash", "strokeDashOffset", "strokeJoin", "strokeMiterLimit", 
  "strokeOpacity", "strokeWidth", "style", "tension", "text", "theta", "thickness", 
  "tooltip", "type", "x", "x2", "x2Offset", "xOffset", "y", "y2", "y2Offset", 
  "yOffset"))
  .add_mark(args$spec, args$object, '#/definitions/MarkDef', args$extra)
} #' vl_mark_geoshape
#'
#'
#' @param spec An input vega-lite spec
#' @param align (_MarkDef_) The horizontal alignment of the text. One of `"left"`, `"right"`, `"center"`.
#' @param angle (_MarkDef_) The rotation angle of the text, in degrees.
#' @param baseline (_MarkDef_) The vertical alignment of the text. One of `"top"`, `"middle"`, `"bottom"`.
#' 
#' __Default value:__ `"middle"`
#' @param binSpacing (_MarkDef_) Offset between bars for binned field.  Ideal value for this is either 0 (Preferred by statisticians) or 1 (Vega-Lite Default, D3 example style).
#' 
#' __Default value:__ `1`
#' @param clip (_MarkDef_) Whether a mark be clipped to the enclosing group’s width and height.
#' @param color (_MarkDef_) Default color.  Note that `fill` and `stroke` have higher precedence than `color` and will override `color`.
#' 
#' __Default value:__ <span style="color: #4682b4;">&#9632;</span> `"#4682b4"`
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param cornerRadius (_MarkDef_) The radius in pixels of rounded rectangle corners.
#' 
#' __Default value:__ `0`
#' @param cursor (_MarkDef_) The mouse cursor used over the mark. Any valid [CSS cursor type](https://developer.mozilla.org/en-US/docs/Web/CSS/cursor#Values) can be used.
#' @param dir (_MarkDef_) The direction of the text. One of `"ltr"` (left-to-right) or `"rtl"` (right-to-left). This property determines on which side is truncated in response to the limit parameter.
#' 
#' __Default value:__ `"ltr"`
#' @param dx (_MarkDef_) The horizontal offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param dy (_MarkDef_) The vertical offset, in pixels, between the text label and its anchor point. The offset is applied after rotation by the _angle_ property.
#' @param ellipsis (_MarkDef_) The ellipsis string for text truncated in response to the limit parameter.
#' 
#' __Default value:__ `"…"`
#' @param fill (_MarkDef_) Default Fill Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param fillOpacity (_MarkDef_) The fill opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param filled (_MarkDef_) Whether the mark's color should be used as fill color instead of stroke color.
#' 
#' __Default value:__ `false` for `point`, `line` and `rule`; otherwise, `true`.
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param font (_MarkDef_) The typeface to set the text in (e.g., `"Helvetica Neue"`).
#' @param fontSize (_MarkDef_) The font size, in pixels.
#' @param fontStyle (_MarkDef_) The font style (e.g., `"italic"`).
#' @param fontWeight (_MarkDef_) The font weight.
#' This can be either a string (e.g `"bold"`, `"normal"`) or a number (`100`, `200`, `300`, ..., `900` where `"normal"` = `400` and `"bold"` = `700`).
#' @param href (_MarkDef_) A URL to load upon mouse click. If defined, the mark acts as a hyperlink.
#' @param interpolate (_MarkDef_) The line interpolation method to use for line and area marks. One of the following:
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
#' - `"monotone"`: cubic interpolation that preserves monotonicity in y.
#' @param limit (_MarkDef_) The maximum length of the text mark in pixels. The text value will be automatically truncated if the rendered size exceeds the limit.
#' 
#' __Default value:__ `0`, indicating no limit
#' @param line (_MarkDef_) A flag for overlaying line on top of area marks, or an object defining the properties of the overlayed lines.
#' 
#' - If this value is an empty object (`{}`) or `true`, lines with default properties will be used.
#' 
#' - If this value is `false`, no lines would be automatically added to area marks.
#' 
#' __Default value:__ `false`.
#' @param opacity (_MarkDef_) The overall opacity (value between \[0,1\]).
#' 
#' __Default value:__ `0.7` for non-aggregate plots with `point`, `tick`, `circle`, or `square` marks or layered `bar` charts and `1` otherwise.
#' @param order (_MarkDef_) For line and trail marks, this `order` property can be set to `null` or `false` to make the lines use the original order in the data sources.
#' @param orient (_MarkDef_) The orientation of a non-stacked bar, tick, area, and line charts.
#' The value is either horizontal (default) or vertical.
#' - For bar, rule and tick, this determines whether the size of the bar and tick
#' should be applied to x or y dimension.
#' - For area, this property determines the orient property of the Vega output.
#' - For line and trail marks, this property determines the sort order of the points in the line
#' if `config.sortLineBy` is not specified.
#' For stacked charts, this is always determined by the orientation of the stack;
#' therefore explicitly specified value will be ignored.
#' @param point (_MarkDef_) A flag for overlaying points on top of line or area marks, or an object defining the properties of the overlayed points.
#' 
#' - If this property is `"transparent"`, transparent points will be used (for enhancing tooltips and selections).
#' 
#' - If this property is an empty object (`{}`) or `true`, filled points with default properties will be used.
#' 
#' - If this property is `false`, no points would be automatically added to line or area marks.
#' 
#' __Default value:__ `false`.
#' @param radius (_MarkDef_) Polar coordinate radial offset, in pixels, of the text label from the origin determined by the `x` and `y` properties.
#' @param shape (_MarkDef_) Shape of the point marks. Supported values include:
#' - plotting shapes: `"circle"`, `"square"`, `"cross"`, `"diamond"`, `"triangle-up"`, `"triangle-down"`, `"triangle-right"`, or `"triangle-left"`.
#' - the line symbol `"stroke"`
#' - centered directional shapes `"arrow"`, `"wedge"`, or `"triangle"`
#' - a custom [SVG path string](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) (For correct sizing, custom shape paths should be defined within a square bounding box with coordinates ranging from -1 to 1 along both the x and y dimensions.)
#' 
#' __Default value:__ `"circle"`
#' @param size (_MarkDef_) Default size for marks.
#' - For `point`/`circle`/`square`, this represents the pixel area of the marks. For example: in the case of circles, the radius is determined in part by the square root of the size value.
#' - For `bar`, this represents the band size of the bar, in pixels.
#' - For `text`, this represents the font size, in pixels.
#' 
#' __Default value:__ `30` for point, circle, square marks; `rangeStep` - 1 for bar marks with discrete dimensions; `5` for bar marks with continuous dimensions; `11` for text marks.
#' @param stroke (_MarkDef_) Default Stroke Color.  This has higher precedence than `config.color`
#' 
#' __Default value:__ (None)
#' @param strokeCap (_MarkDef_) The stroke cap for line ending style. One of `"butt"`, `"round"`, or `"square"`.
#' 
#' __Default value:__ `"square"`
#' @param strokeDash (_MarkDef_) An array of alternating stroke, space lengths for creating dashed or dotted lines.
#' @param strokeDashOffset (_MarkDef_) The offset (in pixels) into which to begin drawing with the stroke dash array.
#' @param strokeJoin (_MarkDef_) The stroke line join method. One of `"miter"`, `"round"` or `"bevel"`.
#' 
#' __Default value:__ `"miter"`
#' @param strokeMiterLimit (_MarkDef_) The miter limit at which to bevel a line join.
#' @param strokeOpacity (_MarkDef_) The stroke opacity (value between \[0,1\]).
#' 
#' __Default value:__ `1`
#' @param strokeWidth (_MarkDef_) The stroke width, in pixels.
#' @param style (_MarkDef_) A string or array of strings indicating the name of custom styles to apply to the mark. A style is a named collection of mark property defaults defined within the [style configuration](https://vega.github.io/vega-lite/docs/mark.html#style-config). If style is an array, later styles will override earlier styles. Any [mark properties](https://vega.github.io/vega-lite/docs/encoding.html#mark-prop) explicitly defined within the `encoding` will override a style default.
#' 
#' __Default value:__ The mark's name.  For example, a bar mark will have style `"bar"` by default.
#' __Note:__ Any specified style will augment the default style. For example, a bar mark with `"style": "foo"` will receive from `config.style.bar` and `config.style.foo` (the specified style `"foo"` has higher precedence).
#' @param tension (_MarkDef_) Depending on the interpolation type, sets the tension parameter (for line and area marks).
#' @param text (_MarkDef_) Placeholder text if the `text` channel is not specified
#' @param theta (_MarkDef_) Polar coordinate angle, in radians, of the text label from the origin determined by the `x` and `y` properties. Values for `theta` follow the same convention of `arc` mark `startAngle` and `endAngle` properties: angles are measured in radians, with `0` indicating "north".
#' @param thickness (_MarkDef_) Thickness of the tick mark.
#' 
#' __Default value:__  `1`
#' @param tooltip (_MarkDef_) The tooltip text string to show upon mouse hover or an object defining which fields should the tooltip be derived from.
#' 
#' - If `tooltip` is `{"content": "encoding"}`, then all fields from `encoding` will be used.
#' - If `tooltip` is `{"content": "data"}`, then all fields that appear in the highlighted data point will be used.
#' - If set to `null`, then no tooltip will be used.
#' @param x (_MarkDef_) X coordinates of the marks, or width of horizontal `"bar"` and `"area"` without `x2`.
#' @param x2 (_MarkDef_) X2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param x2Offset (_MarkDef_) Offset for x2-position.
#' @param xOffset (_MarkDef_) Offset for x-position.
#' @param y (_MarkDef_) Y coordinates of the marks, or height of vertical `"bar"` and `"area"` without `y2`
#' @param y2 (_MarkDef_) Y2 coordinates for ranged `"area"`, `"bar"`, `"rect"`, and  `"rule"`.
#' @param y2Offset (_MarkDef_) Offset for y2-position.
#' @param yOffset (_MarkDef_) Offset for y-position.
#' @return A modified Vega-Lite Spec
#' @export
vl_mark_geoshape <- function(spec, align = NULL, angle = NULL, baseline = NULL, binSpacing = NULL, clip = NULL, color = NULL, cornerRadius = NULL, cursor = NULL, dir = NULL, dx = NULL, dy = NULL, ellipsis = NULL, fill = NULL, fillOpacity = NULL, filled = NULL, font = NULL, fontSize = NULL, fontStyle = NULL, fontWeight = NULL, href = NULL, interpolate = NULL, limit = NULL, line = NULL, opacity = NULL, order = NULL, orient = NULL, point = NULL, radius = NULL, shape = NULL, size = NULL, stroke = NULL, strokeCap = NULL, strokeDash = NULL, strokeDashOffset = NULL, strokeJoin = NULL, strokeMiterLimit = NULL, strokeOpacity = NULL, strokeWidth = NULL, style = NULL, tension = NULL, text = NULL, theta = NULL, thickness = NULL, tooltip = NULL, type = NULL, x = NULL, x2 = NULL, x2Offset = NULL, xOffset = NULL, y = NULL, y2 = NULL, y2Offset = NULL, yOffset = NULL){
  args <- .modify_args(list(type = "geoshape"), c("align", "angle", "baseline", "binSpacing", "clip", "color", "cornerRadius", 
  "cursor", "dir", "dx", "dy", "ellipsis", "fill", "fillOpacity", "filled", "font", 
  "fontSize", "fontStyle", "fontWeight", "href", "interpolate", "limit", "line", 
  "opacity", "order", "orient", "point", "radius", "shape", "size", "stroke", 
  "strokeCap", "strokeDash", "strokeDashOffset", "strokeJoin", "strokeMiterLimit", 
  "strokeOpacity", "strokeWidth", "style", "tension", "text", "theta", "thickness", 
  "tooltip", "type", "x", "x2", "x2Offset", "xOffset", "y", "y2", "y2Offset", 
  "yOffset"))
  .add_mark(args$spec, args$object, '#/definitions/MarkDef', args$extra)
} #' vl_mark_boxplot
#'
#'
#' @param spec An input vega-lite spec
#' @param box (_BoxPlotDef_)  
#' @param clip (_BoxPlotDef_) Whether a composite mark be clipped to the enclosing group’s width and height.
#' @param color (_BoxPlotDef_) Default color.  Note that `fill` and `stroke` have higher precedence than `color` and will override `color`.
#' 
#' __Default value:__ <span style="color: #4682b4;">&#9632;</span> `"#4682b4"`
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param extent (_BoxPlotDef_) The extent of the whiskers. Available options include:
#' - `"min-max"`: min and max are the lower and upper whiskers respectively.
#' - A number representing multiple of the interquartile range.  This number will be multiplied by the IQR to determine whisker boundary, which spans from the smallest data to the largest data within the range _\[Q1 - k * IQR, Q3 + k * IQR\]_ where _Q1_ and _Q3_ are the first and third quartiles while _IQR_ is the interquartile range (_Q3-Q1_).
#' 
#' __Default value:__ `1.5`.
#' @param median (_BoxPlotDef_)  
#' @param opacity (_BoxPlotDef_) The opacity (value between \[0,1\]) of the mark.
#' @param orient (_BoxPlotDef_) Orientation of the box plot.  This is normally automatically determined based on types of fields on x and y channels. However, an explicit `orient` be specified when the orientation is ambiguous.
#' 
#' __Default value:__ `"vertical"`.
#' @param outliers (_BoxPlotDef_)  
#' @param rule (_BoxPlotDef_)  
#' @param size (_BoxPlotDef_) Size of the box and median tick of a box plot
#' @param ticks (_BoxPlotDef_)  
#' @return A modified Vega-Lite Spec
#' @export
vl_mark_boxplot <- function(spec, box = NULL, clip = NULL, color = NULL, extent = NULL, median = NULL, opacity = NULL, orient = NULL, outliers = NULL, rule = NULL, size = NULL, ticks = NULL, type = NULL){
  args <- .modify_args(list(type = "boxplot"), c("box", "clip", "color", "extent", "median", "opacity", "orient", "outliers", 
  "rule", "size", "ticks", "type"))
  .add_mark(args$spec, args$object, '#/definitions/BoxPlotDef', args$extra)
} #' vl_mark_errorbar
#'
#'
#' @param spec An input vega-lite spec
#' @param clip (_ErrorBarDef_) Whether a composite mark be clipped to the enclosing group’s width and height.
#' @param color (_ErrorBarDef_) Default color.  Note that `fill` and `stroke` have higher precedence than `color` and will override `color`.
#' 
#' __Default value:__ <span style="color: #4682b4;">&#9632;</span> `"#4682b4"`
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param extent (_ErrorBarDef_) The extent of the rule. Available options include:
#' - `"ci"`: Extend the rule to the confidence interval of the mean.
#' - `"stderr"`: The size of rule are set to the value of standard error, extending from the mean.
#' - `"stdev"`: The size of rule are set to the value of standard deviation, extending from the mean.
#' - `"iqr"`: Extend the rule to the q1 and q3.
#' 
#' __Default value:__ `"stderr"`.
#' @param opacity (_ErrorBarDef_) The opacity (value between \[0,1\]) of the mark.
#' @param orient (_ErrorBarDef_) Orientation of the error bar.  This is normally automatically determined, but can be specified when the orientation is ambiguous and cannot be automatically determined.
#' @param rule (_ErrorBarDef_)  
#' @param ticks (_ErrorBarDef_)  
#' @return A modified Vega-Lite Spec
#' @export
vl_mark_errorbar <- function(spec, clip = NULL, color = NULL, extent = NULL, opacity = NULL, orient = NULL, rule = NULL, ticks = NULL, type = NULL){
  args <- .modify_args(list(type = "errorbar"), c("clip", "color", "extent", "opacity", "orient", "rule", "ticks", "type"))
  .add_mark(args$spec, args$object, '#/definitions/ErrorBarDef', args$extra)
} #' vl_mark_errorband
#'
#'
#' @param spec An input vega-lite spec
#' @param band (_ErrorBandDef_)  
#' @param borders (_ErrorBandDef_)  
#' @param clip (_ErrorBandDef_) Whether a composite mark be clipped to the enclosing group’s width and height.
#' @param color (_ErrorBandDef_) Default color.  Note that `fill` and `stroke` have higher precedence than `color` and will override `color`.
#' 
#' __Default value:__ <span style="color: #4682b4;">&#9632;</span> `"#4682b4"`
#' 
#' __Note:__ This property cannot be used in a [style config](https://vega.github.io/vega-lite/docs/mark.html#style-config).
#' @param extent (_ErrorBandDef_) The extent of the band. Available options include:
#' - `"ci"`: Extend the band to the confidence interval of the mean.
#' - `"stderr"`: The size of band are set to the value of standard error, extending from the mean.
#' - `"stdev"`: The size of band are set to the value of standard deviation, extending from the mean.
#' - `"iqr"`: Extend the band to the q1 and q3.
#' 
#' __Default value:__ `"stderr"`.
#' @param interpolate (_ErrorBandDef_) The line interpolation method for the error band. One of the following:
#' - `"linear"`: piecewise linear segments, as in a polyline.
#' - `"linear-closed"`: close the linear segments to form a polygon.
#' - `"step"`: a piecewise constant function (a step function) consisting of alternating horizontal and vertical lines. The y-value changes at the midpoint of each pair of adjacent x-values.
#' - `"step-before"`: a piecewise constant function (a step function) consisting of alternating horizontal and vertical lines. The y-value changes before the x-value.
#' - `"step-after"`: a piecewise constant function (a step function) consisting of alternating horizontal and vertical lines. The y-value changes after the x-value.
#' - `"basis"`: a B-spline, with control point duplication on the ends.
#' - `"basis-open"`: an open B-spline; may not intersect the start or end.
#' - `"basis-closed"`: a closed B-spline, as in a loop.
#' - `"cardinal"`: a Cardinal spline, with control point duplication on the ends.
#' - `"cardinal-open"`: an open Cardinal spline; may not intersect the start or end, but will intersect other control points.
#' - `"cardinal-closed"`: a closed Cardinal spline, as in a loop.
#' - `"bundle"`: equivalent to basis, except the tension parameter is used to straighten the spline.
#' - `"monotone"`: cubic interpolation that preserves monotonicity in y.
#' @param opacity (_ErrorBandDef_) The opacity (value between \[0,1\]) of the mark.
#' @param orient (_ErrorBandDef_) Orientation of the error band. This is normally automatically determined, but can be specified when the orientation is ambiguous and cannot be automatically determined.
#' @param tension (_ErrorBandDef_) The tension parameter for the interpolation type of the error band.
#' @return A modified Vega-Lite Spec
#' @export
vl_mark_errorband <- function(spec, band = NULL, borders = NULL, clip = NULL, color = NULL, extent = NULL, interpolate = NULL, opacity = NULL, orient = NULL, tension = NULL, type = NULL){
  args <- .modify_args(list(type = "errorband"), c("band", "borders", "clip", "color", "extent", "interpolate", "opacity", "orient", 
  "tension", "type"))
  .add_mark(args$spec, args$object, '#/definitions/ErrorBandDef', args$extra)
} #' vl_encode_color
#'
#' Add encoding for color to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<MarkPropFieldDef,(string|null)>, ConditionOnlyDef<MarkPropFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param legend (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied.
#' @param scale (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param timeUnit (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<MarkPropFieldDef,(string|null)>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_color <- function(spec, field = NULL, type = NULL, aggregate = NULL, bin = NULL, condition = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "condition", "field", "legend", "scale", "sort", "timeUnit", 
  "title", "type", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/color', args$extra, encoding = "color")
} #' vl_encode_detail
#'
#' Add encoding for detail to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_FieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_FieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_FieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDef_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_detail <- function(spec, field = NULL, type = NULL, aggregate = NULL, bin = NULL, timeUnit = NULL, title = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "field", "timeUnit", "title", "type"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/detail', args$extra, encoding = "detail")
} #' vl_encode_fill
#'
#' Add encoding for fill to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<MarkPropFieldDef,(string|null)>, ConditionOnlyDef<MarkPropFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param legend (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied.
#' @param scale (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param timeUnit (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<MarkPropFieldDef,(string|null)>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_fill <- function(spec, field = NULL, type = NULL, aggregate = NULL, bin = NULL, condition = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "condition", "field", "legend", "scale", "sort", "timeUnit", 
  "title", "type", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/fill', args$extra, encoding = "fill")
} #' vl_encode_fillOpacity
#'
#' Add encoding for fillOpacity to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_FieldDefWithCondition<MarkPropFieldDef,number>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<MarkPropFieldDef,number>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>, ConditionOnlyDef<MarkPropFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<MarkPropFieldDef,number>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<MarkPropFieldDef,number>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param legend (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied.
#' @param scale (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_FieldDefWithCondition<MarkPropFieldDef,number>_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param timeUnit (_FieldDefWithCondition<MarkPropFieldDef,number>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<MarkPropFieldDef,number>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<MarkPropFieldDef,number>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_fillOpacity <- function(spec, field = NULL, type = NULL, aggregate = NULL, bin = NULL, condition = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "condition", "field", "legend", "scale", "sort", "timeUnit", 
  "title", "type", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/fillOpacity', args$extra, encoding = "fillOpacity")
} #' vl_encode_href
#'
#' Add encoding for href to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<TextFieldDef,(string|number|boolean)>, ConditionOnlyDef<TextFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param format (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) The text formatting pattern for labels of guides (axes, legends, headers) and text marks.
#' 
#' - If the format type is `"number"` (e.g., for quantitative fields), this is D3's [number format pattern](https://github.com/d3/d3-format#locale_format).
#' - If the format type is `"time"` (e.g., for temporal fields), this is D3's [time format pattern](https://github.com/d3/d3-time-format#locale_format).
#' 
#' See the [format documentation](https://vega.github.io/vega-lite/docs/format.html) for more examples.
#' 
#' __Default value:__  Derived from [numberFormat](https://vega.github.io/vega-lite/docs/config.html#format) config for number format and from [timeFormat](https://vega.github.io/vega-lite/docs/config.html#format) config for time format.
#' @param formatType (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) The format type for labels (`"number"` or `"time"`).
#' 
#' __Default value:__
#' - `"time"` for temporal fields and ordinal and nomimal fields with `timeUnit`.
#' - `"number"` for quantitative fields as well as ordinal and nomimal fields without `timeUnit`.
#' @param timeUnit (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<TextFieldDef,(string|number|boolean)>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_href <- function(spec, field = NULL, type = NULL, aggregate = NULL, bin = NULL, condition = NULL, format = NULL, formatType = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "condition", "field", "format", "formatType", "timeUnit", 
  "title", "type", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/href', args$extra, encoding = "href")
} #' vl_encode_key
#'
#' Add encoding for key to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_FieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_FieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_FieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDef_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_key <- function(spec, field = NULL, type = NULL, aggregate = NULL, bin = NULL, timeUnit = NULL, title = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "field", "timeUnit", "title", "type"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/key', args$extra, encoding = "key")
} #' vl_encode_latitude
#'
#' Add encoding for latitude to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_LatLongFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_LatLongFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_LatLongFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_LatLongFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_LatLongFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_LatLongFieldDef_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_NumberValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_latitude <- function(spec, field = NULL, type = NULL, aggregate = NULL, bin = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "field", "timeUnit", "title", "type", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/latitude', args$extra, encoding = "latitude")
} #' vl_encode_latitude2
#'
#' Add encoding for latitude2 to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_SecondaryFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_SecondaryFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_SecondaryFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_SecondaryFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_SecondaryFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param value (_NumberValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_latitude2 <- function(spec, field = NULL, aggregate = NULL, bin = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "field", "timeUnit", "title", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/latitude2', args$extra, encoding = "latitude2")
} #' vl_encode_longitude
#'
#' Add encoding for longitude to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_LatLongFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_LatLongFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_LatLongFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_LatLongFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_LatLongFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_LatLongFieldDef_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_NumberValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_longitude <- function(spec, field = NULL, type = NULL, aggregate = NULL, bin = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "field", "timeUnit", "title", "type", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/longitude', args$extra, encoding = "longitude")
} #' vl_encode_longitude2
#'
#' Add encoding for longitude2 to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_SecondaryFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_SecondaryFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_SecondaryFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_SecondaryFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_SecondaryFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param value (_NumberValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_longitude2 <- function(spec, field = NULL, aggregate = NULL, bin = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "field", "timeUnit", "title", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/longitude2', args$extra, encoding = "longitude2")
} #' vl_encode_opacity
#'
#' Add encoding for opacity to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_FieldDefWithCondition<MarkPropFieldDef,number>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<MarkPropFieldDef,number>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>, ConditionOnlyDef<MarkPropFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<MarkPropFieldDef,number>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<MarkPropFieldDef,number>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param legend (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied.
#' @param scale (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_FieldDefWithCondition<MarkPropFieldDef,number>_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param timeUnit (_FieldDefWithCondition<MarkPropFieldDef,number>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<MarkPropFieldDef,number>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<MarkPropFieldDef,number>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_opacity <- function(spec, field = NULL, type = NULL, aggregate = NULL, bin = NULL, condition = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "condition", "field", "legend", "scale", "sort", "timeUnit", 
  "title", "type", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/opacity', args$extra, encoding = "opacity")
} #' vl_encode_order
#'
#' Add encoding for order to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_OrderFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_OrderFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_OrderFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param sort (_OrderFieldDef_) The sort order. One of `"ascending"` (default) or `"descending"`.
#' @param timeUnit (_OrderFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_OrderFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_OrderFieldDef_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_NumberValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_order <- function(spec, field = NULL, type = NULL, aggregate = NULL, bin = NULL, sort = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "field", "sort", "timeUnit", "title", "type", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/order', args$extra, encoding = "order")
} #' vl_encode_shape
#'
#' Add encoding for shape to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<MarkPropFieldDef<TypeForShape>,string>, ConditionOnlyDef<MarkPropFieldDef<TypeForShape>>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param legend (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied.
#' @param scale (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param timeUnit (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<MarkPropFieldDef<TypeForShape>,string>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_shape <- function(spec, field = NULL, type = NULL, aggregate = NULL, bin = NULL, condition = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "condition", "field", "legend", "scale", "sort", "timeUnit", 
  "title", "type", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/shape', args$extra, encoding = "shape")
} #' vl_encode_size
#'
#' Add encoding for size to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_FieldDefWithCondition<MarkPropFieldDef,number>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<MarkPropFieldDef,number>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>, ConditionOnlyDef<MarkPropFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<MarkPropFieldDef,number>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<MarkPropFieldDef,number>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param legend (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied.
#' @param scale (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_FieldDefWithCondition<MarkPropFieldDef,number>_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param timeUnit (_FieldDefWithCondition<MarkPropFieldDef,number>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<MarkPropFieldDef,number>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<MarkPropFieldDef,number>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_size <- function(spec, field = NULL, type = NULL, aggregate = NULL, bin = NULL, condition = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "condition", "field", "legend", "scale", "sort", "timeUnit", 
  "title", "type", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/size', args$extra, encoding = "size")
} #' vl_encode_stroke
#'
#' Add encoding for stroke to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<MarkPropFieldDef,(string|null)>, ConditionOnlyDef<MarkPropFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param legend (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied.
#' @param scale (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param timeUnit (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<MarkPropFieldDef,(string|null)>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_stroke <- function(spec, field = NULL, type = NULL, aggregate = NULL, bin = NULL, condition = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "condition", "field", "legend", "scale", "sort", "timeUnit", 
  "title", "type", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/stroke', args$extra, encoding = "stroke")
} #' vl_encode_strokeOpacity
#'
#' Add encoding for strokeOpacity to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_FieldDefWithCondition<MarkPropFieldDef,number>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<MarkPropFieldDef,number>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>, ConditionOnlyDef<MarkPropFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<MarkPropFieldDef,number>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<MarkPropFieldDef,number>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param legend (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied.
#' @param scale (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_FieldDefWithCondition<MarkPropFieldDef,number>_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param timeUnit (_FieldDefWithCondition<MarkPropFieldDef,number>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<MarkPropFieldDef,number>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<MarkPropFieldDef,number>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_strokeOpacity <- function(spec, field = NULL, type = NULL, aggregate = NULL, bin = NULL, condition = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "condition", "field", "legend", "scale", "sort", "timeUnit", 
  "title", "type", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/strokeOpacity', args$extra, encoding = "strokeOpacity")
} #' vl_encode_strokeWidth
#'
#' Add encoding for strokeWidth to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_FieldDefWithCondition<MarkPropFieldDef,number>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<MarkPropFieldDef,number>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>, ConditionOnlyDef<MarkPropFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<MarkPropFieldDef,number>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<MarkPropFieldDef,number>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param legend (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied.
#' @param scale (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_FieldDefWithCondition<MarkPropFieldDef,number>_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param timeUnit (_FieldDefWithCondition<MarkPropFieldDef,number>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<MarkPropFieldDef,number>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<MarkPropFieldDef,number>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_strokeWidth <- function(spec, field = NULL, type = NULL, aggregate = NULL, bin = NULL, condition = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "condition", "field", "legend", "scale", "sort", "timeUnit", 
  "title", "type", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/strokeWidth', args$extra, encoding = "strokeWidth")
} #' vl_encode_text
#'
#' Add encoding for text to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<TextFieldDef,(string|number|boolean)>, ConditionOnlyDef<TextFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param format (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) The text formatting pattern for labels of guides (axes, legends, headers) and text marks.
#' 
#' - If the format type is `"number"` (e.g., for quantitative fields), this is D3's [number format pattern](https://github.com/d3/d3-format#locale_format).
#' - If the format type is `"time"` (e.g., for temporal fields), this is D3's [time format pattern](https://github.com/d3/d3-time-format#locale_format).
#' 
#' See the [format documentation](https://vega.github.io/vega-lite/docs/format.html) for more examples.
#' 
#' __Default value:__  Derived from [numberFormat](https://vega.github.io/vega-lite/docs/config.html#format) config for number format and from [timeFormat](https://vega.github.io/vega-lite/docs/config.html#format) config for time format.
#' @param formatType (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) The format type for labels (`"number"` or `"time"`).
#' 
#' __Default value:__
#' - `"time"` for temporal fields and ordinal and nomimal fields with `timeUnit`.
#' - `"number"` for quantitative fields as well as ordinal and nomimal fields without `timeUnit`.
#' @param timeUnit (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<TextFieldDef,(string|number|boolean)>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_text <- function(spec, field = NULL, type = NULL, aggregate = NULL, bin = NULL, condition = NULL, format = NULL, formatType = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "condition", "field", "format", "formatType", "timeUnit", 
  "title", "type", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/text', args$extra, encoding = "text")
} #' vl_encode_tooltip
#'
#' Add encoding for tooltip to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<TextFieldDef,(string|number|boolean)>, ConditionOnlyDef<TextFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param format (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) The text formatting pattern for labels of guides (axes, legends, headers) and text marks.
#' 
#' - If the format type is `"number"` (e.g., for quantitative fields), this is D3's [number format pattern](https://github.com/d3/d3-format#locale_format).
#' - If the format type is `"time"` (e.g., for temporal fields), this is D3's [time format pattern](https://github.com/d3/d3-time-format#locale_format).
#' 
#' See the [format documentation](https://vega.github.io/vega-lite/docs/format.html) for more examples.
#' 
#' __Default value:__  Derived from [numberFormat](https://vega.github.io/vega-lite/docs/config.html#format) config for number format and from [timeFormat](https://vega.github.io/vega-lite/docs/config.html#format) config for time format.
#' @param formatType (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) The format type for labels (`"number"` or `"time"`).
#' 
#' __Default value:__
#' - `"time"` for temporal fields and ordinal and nomimal fields with `timeUnit`.
#' - `"number"` for quantitative fields as well as ordinal and nomimal fields without `timeUnit`.
#' @param timeUnit (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<TextFieldDef,(string|number|boolean)>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_tooltip <- function(spec, field = NULL, type = NULL, aggregate = NULL, bin = NULL, condition = NULL, format = NULL, formatType = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "condition", "field", "format", "formatType", "timeUnit", 
  "title", "type", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/tooltip', args$extra, encoding = "tooltip")
} #' vl_encode_x
#'
#' Add encoding for x to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_PositionFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param axis (_PositionFieldDef_) An object defining properties of axis's gridlines, ticks and labels.
#' If `null`, the axis for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [axis properties](https://vega.github.io/vega-lite/docs/axis.html) are applied.
#' @param bin (_PositionFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_PositionFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param impute (_PositionFieldDef_) An object defining the properties of the Impute Operation to be applied.
#' The field value of the other positional channel is taken as `key` of the `Impute` Operation.
#' The field of the `color` channel if specified is used as `groupby` of the `Impute` Operation.
#' @param scale (_PositionFieldDef_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_PositionFieldDef_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param stack (_PositionFieldDef_) Type of stacking offset if the field should be stacked.
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
#' (3) At least one of non-position channels mapped to an unaggregated field that is different from x and y.  Otherwise, `null` by default.
#' @param timeUnit (_PositionFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_PositionFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_PositionFieldDef_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_XValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_x <- function(spec, field = NULL, type = NULL, aggregate = NULL, axis = NULL, bin = NULL, impute = NULL, scale = NULL, sort = NULL, stack = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "axis", "bin", "field", "impute", "scale", "sort", "stack", "timeUnit", 
  "title", "type", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/x', args$extra, encoding = "x")
} #' vl_encode_x2
#'
#' Add encoding for x2 to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_SecondaryFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_SecondaryFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_SecondaryFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_SecondaryFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_SecondaryFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param value (_XValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_x2 <- function(spec, field = NULL, aggregate = NULL, bin = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "field", "timeUnit", "title", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/x2', args$extra, encoding = "x2")
} #' vl_encode_xError
#'
#' Add encoding for xError to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_SecondaryFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_SecondaryFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_SecondaryFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_SecondaryFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_SecondaryFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param value (_NumberValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_xError <- function(spec, field = NULL, aggregate = NULL, bin = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "field", "timeUnit", "title", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/xError', args$extra, encoding = "xError")
} #' vl_encode_xError2
#'
#' Add encoding for xError2 to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_SecondaryFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_SecondaryFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_SecondaryFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_SecondaryFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_SecondaryFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param value (_NumberValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_xError2 <- function(spec, field = NULL, aggregate = NULL, bin = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "field", "timeUnit", "title", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/xError2', args$extra, encoding = "xError2")
} #' vl_encode_y
#'
#' Add encoding for y to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_PositionFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param axis (_PositionFieldDef_) An object defining properties of axis's gridlines, ticks and labels.
#' If `null`, the axis for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [axis properties](https://vega.github.io/vega-lite/docs/axis.html) are applied.
#' @param bin (_PositionFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_PositionFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param impute (_PositionFieldDef_) An object defining the properties of the Impute Operation to be applied.
#' The field value of the other positional channel is taken as `key` of the `Impute` Operation.
#' The field of the `color` channel if specified is used as `groupby` of the `Impute` Operation.
#' @param scale (_PositionFieldDef_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_PositionFieldDef_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param stack (_PositionFieldDef_) Type of stacking offset if the field should be stacked.
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
#' (3) At least one of non-position channels mapped to an unaggregated field that is different from x and y.  Otherwise, `null` by default.
#' @param timeUnit (_PositionFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_PositionFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_PositionFieldDef_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_YValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_y <- function(spec, field = NULL, type = NULL, aggregate = NULL, axis = NULL, bin = NULL, impute = NULL, scale = NULL, sort = NULL, stack = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "axis", "bin", "field", "impute", "scale", "sort", "stack", "timeUnit", 
  "title", "type", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/y', args$extra, encoding = "y")
} #' vl_encode_y2
#'
#' Add encoding for y2 to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_SecondaryFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_SecondaryFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_SecondaryFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_SecondaryFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_SecondaryFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param value (_YValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_y2 <- function(spec, field = NULL, aggregate = NULL, bin = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "field", "timeUnit", "title", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/y2', args$extra, encoding = "y2")
} #' vl_encode_yError
#'
#' Add encoding for yError to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_SecondaryFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_SecondaryFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_SecondaryFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_SecondaryFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_SecondaryFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param value (_NumberValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_yError <- function(spec, field = NULL, aggregate = NULL, bin = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "field", "timeUnit", "title", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/yError', args$extra, encoding = "yError")
} #' vl_encode_yError2
#'
#' Add encoding for yError2 to a vega-lite spec.
#' @param spec An input vega-lite spec
#' @param aggregate (_SecondaryFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_SecondaryFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_SecondaryFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_SecondaryFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_SecondaryFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param value (_NumberValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified Vega-Lite Spec
#' @export
vl_encode_yError2 <- function(spec, field = NULL, aggregate = NULL, bin = NULL, timeUnit = NULL, title = NULL, value = NULL){
  args <- .modify_args(NULL, c("aggregate", "bin", "field", "timeUnit", "title", "value"))
  .add_encoding(args$spec, args$object, '#/definitions/Encoding/properties/yError2', args$extra, encoding = "yError2")
} #' vl_make_Color
#' 
#' Create spec for Color.
#' @param aggregate (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<MarkPropFieldDef,(string|null)>, ConditionOnlyDef<MarkPropFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param legend (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied.
#' @param scale (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param timeUnit (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<MarkPropFieldDef,(string|null)>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_Color <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_Detail
#' 
#' Create spec for Detail.
#' @param aggregate (_FieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_FieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_FieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDef_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @return A modified spec
#' @export
vl_make_Detail <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_Fill
#' 
#' Create spec for Fill.
#' @param aggregate (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<MarkPropFieldDef,(string|null)>, ConditionOnlyDef<MarkPropFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param legend (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied.
#' @param scale (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param timeUnit (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<MarkPropFieldDef,(string|null)>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_Fill <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_FillOpacity
#' 
#' Create spec for FillOpacity.
#' @param aggregate (_FieldDefWithCondition<MarkPropFieldDef,number>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<MarkPropFieldDef,number>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>, ConditionOnlyDef<MarkPropFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<MarkPropFieldDef,number>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<MarkPropFieldDef,number>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param legend (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied.
#' @param scale (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_FieldDefWithCondition<MarkPropFieldDef,number>_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param timeUnit (_FieldDefWithCondition<MarkPropFieldDef,number>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<MarkPropFieldDef,number>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<MarkPropFieldDef,number>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_FillOpacity <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_Href
#' 
#' Create spec for Href.
#' @param aggregate (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<TextFieldDef,(string|number|boolean)>, ConditionOnlyDef<TextFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param format (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) The text formatting pattern for labels of guides (axes, legends, headers) and text marks.
#' 
#' - If the format type is `"number"` (e.g., for quantitative fields), this is D3's [number format pattern](https://github.com/d3/d3-format#locale_format).
#' - If the format type is `"time"` (e.g., for temporal fields), this is D3's [time format pattern](https://github.com/d3/d3-time-format#locale_format).
#' 
#' See the [format documentation](https://vega.github.io/vega-lite/docs/format.html) for more examples.
#' 
#' __Default value:__  Derived from [numberFormat](https://vega.github.io/vega-lite/docs/config.html#format) config for number format and from [timeFormat](https://vega.github.io/vega-lite/docs/config.html#format) config for time format.
#' @param formatType (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) The format type for labels (`"number"` or `"time"`).
#' 
#' __Default value:__
#' - `"time"` for temporal fields and ordinal and nomimal fields with `timeUnit`.
#' - `"number"` for quantitative fields as well as ordinal and nomimal fields without `timeUnit`.
#' @param timeUnit (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<TextFieldDef,(string|number|boolean)>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_Href <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, format = NULL, formatType = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_Key
#' 
#' Create spec for Key.
#' @param aggregate (_FieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_FieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_FieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDef_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @return A modified spec
#' @export
vl_make_Key <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_Latitude
#' 
#' Create spec for Latitude.
#' @param aggregate (_LatLongFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_LatLongFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_LatLongFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_LatLongFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_LatLongFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_LatLongFieldDef_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_NumberValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_Latitude <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_Latitude2
#' 
#' Create spec for Latitude2.
#' @param aggregate (_SecondaryFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_SecondaryFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_SecondaryFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_SecondaryFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_SecondaryFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param value (_NumberValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_Latitude2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_Longitude
#' 
#' Create spec for Longitude.
#' @param aggregate (_LatLongFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_LatLongFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_LatLongFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_LatLongFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_LatLongFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_LatLongFieldDef_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_NumberValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_Longitude <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_Longitude2
#' 
#' Create spec for Longitude2.
#' @param aggregate (_SecondaryFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_SecondaryFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_SecondaryFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_SecondaryFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_SecondaryFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param value (_NumberValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_Longitude2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_Opacity
#' 
#' Create spec for Opacity.
#' @param aggregate (_FieldDefWithCondition<MarkPropFieldDef,number>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<MarkPropFieldDef,number>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>, ConditionOnlyDef<MarkPropFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<MarkPropFieldDef,number>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<MarkPropFieldDef,number>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param legend (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied.
#' @param scale (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_FieldDefWithCondition<MarkPropFieldDef,number>_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param timeUnit (_FieldDefWithCondition<MarkPropFieldDef,number>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<MarkPropFieldDef,number>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<MarkPropFieldDef,number>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_Opacity <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_Order
#' 
#' Create spec for Order.
#' @param aggregate (_OrderFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_OrderFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_OrderFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param sort (_OrderFieldDef_) The sort order. One of `"ascending"` (default) or `"descending"`.
#' @param timeUnit (_OrderFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_OrderFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_OrderFieldDef_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_NumberValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_Order <- function(aggregate = NULL, bin = NULL, field = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_Shape
#' 
#' Create spec for Shape.
#' @param aggregate (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<MarkPropFieldDef<TypeForShape>,string>, ConditionOnlyDef<MarkPropFieldDef<TypeForShape>>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param legend (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied.
#' @param scale (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param timeUnit (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<MarkPropFieldDef<TypeForShape>,string>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<MarkPropFieldDef<TypeForShape>,string>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_Shape <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_Size
#' 
#' Create spec for Size.
#' @param aggregate (_FieldDefWithCondition<MarkPropFieldDef,number>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<MarkPropFieldDef,number>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>, ConditionOnlyDef<MarkPropFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<MarkPropFieldDef,number>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<MarkPropFieldDef,number>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param legend (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied.
#' @param scale (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_FieldDefWithCondition<MarkPropFieldDef,number>_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param timeUnit (_FieldDefWithCondition<MarkPropFieldDef,number>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<MarkPropFieldDef,number>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<MarkPropFieldDef,number>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_Size <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_Stroke
#' 
#' Create spec for Stroke.
#' @param aggregate (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<MarkPropFieldDef,(string|null)>, ConditionOnlyDef<MarkPropFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param legend (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied.
#' @param scale (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param timeUnit (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<MarkPropFieldDef,(string|null)>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<MarkPropFieldDef,(string|null)>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_Stroke <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_StrokeOpacity
#' 
#' Create spec for StrokeOpacity.
#' @param aggregate (_FieldDefWithCondition<MarkPropFieldDef,number>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<MarkPropFieldDef,number>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>, ConditionOnlyDef<MarkPropFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<MarkPropFieldDef,number>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<MarkPropFieldDef,number>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param legend (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied.
#' @param scale (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_FieldDefWithCondition<MarkPropFieldDef,number>_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param timeUnit (_FieldDefWithCondition<MarkPropFieldDef,number>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<MarkPropFieldDef,number>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<MarkPropFieldDef,number>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_StrokeOpacity <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_StrokeWidth
#' 
#' Create spec for StrokeWidth.
#' @param aggregate (_FieldDefWithCondition<MarkPropFieldDef,number>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<MarkPropFieldDef,number>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>, ConditionOnlyDef<MarkPropFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<MarkPropFieldDef,number>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<MarkPropFieldDef,number>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param legend (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the legend.
#' If `null`, the legend for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [legend properties](https://vega.github.io/vega-lite/docs/legend.html) are applied.
#' @param scale (_FieldDefWithCondition<MarkPropFieldDef,number>_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_FieldDefWithCondition<MarkPropFieldDef,number>_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param timeUnit (_FieldDefWithCondition<MarkPropFieldDef,number>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<MarkPropFieldDef,number>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<MarkPropFieldDef,number>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<MarkPropFieldDef,number>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_StrokeWidth <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, legend = NULL, scale = NULL, sort = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_Text
#' 
#' Create spec for Text.
#' @param aggregate (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<TextFieldDef,(string|number|boolean)>, ConditionOnlyDef<TextFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param format (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) The text formatting pattern for labels of guides (axes, legends, headers) and text marks.
#' 
#' - If the format type is `"number"` (e.g., for quantitative fields), this is D3's [number format pattern](https://github.com/d3/d3-format#locale_format).
#' - If the format type is `"time"` (e.g., for temporal fields), this is D3's [time format pattern](https://github.com/d3/d3-time-format#locale_format).
#' 
#' See the [format documentation](https://vega.github.io/vega-lite/docs/format.html) for more examples.
#' 
#' __Default value:__  Derived from [numberFormat](https://vega.github.io/vega-lite/docs/config.html#format) config for number format and from [timeFormat](https://vega.github.io/vega-lite/docs/config.html#format) config for time format.
#' @param formatType (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) The format type for labels (`"number"` or `"time"`).
#' 
#' __Default value:__
#' - `"time"` for temporal fields and ordinal and nomimal fields with `timeUnit`.
#' - `"number"` for quantitative fields as well as ordinal and nomimal fields without `timeUnit`.
#' @param timeUnit (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<TextFieldDef,(string|number|boolean)>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_Text <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, format = NULL, formatType = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_Tooltip
#' 
#' Create spec for Tooltip.
#' @param aggregate (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param condition (_ValueDefWithOptionalCondition<TextFieldDef,(string|number|boolean)>, ConditionOnlyDef<TextFieldDef>_) A field definition or one or more value definition(s) with a selection predicate.
#' 
#' (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) One or more value definition(s) with [a selection or a test predicate](https://vega.github.io/vega-lite/docs/condition.html).
#' 
#' __Note:__ A field definition's `condition` property can only contain [conditional value definitions](https://vega.github.io/vega-lite/docs/condition.html#value)
#' since Vega-Lite only allows at most one encoded field per encoding channel.
#' @param field (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param format (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) The text formatting pattern for labels of guides (axes, legends, headers) and text marks.
#' 
#' - If the format type is `"number"` (e.g., for quantitative fields), this is D3's [number format pattern](https://github.com/d3/d3-format#locale_format).
#' - If the format type is `"time"` (e.g., for temporal fields), this is D3's [time format pattern](https://github.com/d3/d3-time-format#locale_format).
#' 
#' See the [format documentation](https://vega.github.io/vega-lite/docs/format.html) for more examples.
#' 
#' __Default value:__  Derived from [numberFormat](https://vega.github.io/vega-lite/docs/config.html#format) config for number format and from [timeFormat](https://vega.github.io/vega-lite/docs/config.html#format) config for time format.
#' @param formatType (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) The format type for labels (`"number"` or `"time"`).
#' 
#' __Default value:__
#' - `"time"` for temporal fields and ordinal and nomimal fields with `timeUnit`.
#' - `"number"` for quantitative fields as well as ordinal and nomimal fields without `timeUnit`.
#' @param timeUnit (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_FieldDefWithCondition<TextFieldDef,(string|number|boolean)>_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_ValueDefWithOptionalCondition<TextFieldDef,(string|number|boolean)>_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_Tooltip <- function(aggregate = NULL, bin = NULL, condition = NULL, field = NULL, format = NULL, formatType = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_X
#' 
#' Create spec for X.
#' @param aggregate (_PositionFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param axis (_PositionFieldDef_) An object defining properties of axis's gridlines, ticks and labels.
#' If `null`, the axis for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [axis properties](https://vega.github.io/vega-lite/docs/axis.html) are applied.
#' @param bin (_PositionFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_PositionFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param impute (_PositionFieldDef_) An object defining the properties of the Impute Operation to be applied.
#' The field value of the other positional channel is taken as `key` of the `Impute` Operation.
#' The field of the `color` channel if specified is used as `groupby` of the `Impute` Operation.
#' @param scale (_PositionFieldDef_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_PositionFieldDef_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param stack (_PositionFieldDef_) Type of stacking offset if the field should be stacked.
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
#' (3) At least one of non-position channels mapped to an unaggregated field that is different from x and y.  Otherwise, `null` by default.
#' @param timeUnit (_PositionFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_PositionFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_PositionFieldDef_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_XValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_X <- function(aggregate = NULL, axis = NULL, bin = NULL, field = NULL, impute = NULL, scale = NULL, sort = NULL, stack = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_X2
#' 
#' Create spec for X2.
#' @param aggregate (_SecondaryFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_SecondaryFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_SecondaryFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_SecondaryFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_SecondaryFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param value (_XValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_X2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_XError
#' 
#' Create spec for XError.
#' @param aggregate (_SecondaryFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_SecondaryFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_SecondaryFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_SecondaryFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_SecondaryFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param value (_NumberValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_XError <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_XError2
#' 
#' Create spec for XError2.
#' @param aggregate (_SecondaryFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_SecondaryFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_SecondaryFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_SecondaryFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_SecondaryFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param value (_NumberValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_XError2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_Y
#' 
#' Create spec for Y.
#' @param aggregate (_PositionFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param axis (_PositionFieldDef_) An object defining properties of axis's gridlines, ticks and labels.
#' If `null`, the axis for the encoding channel will be removed.
#' 
#' __Default value:__ If undefined, default [axis properties](https://vega.github.io/vega-lite/docs/axis.html) are applied.
#' @param bin (_PositionFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_PositionFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param impute (_PositionFieldDef_) An object defining the properties of the Impute Operation to be applied.
#' The field value of the other positional channel is taken as `key` of the `Impute` Operation.
#' The field of the `color` channel if specified is used as `groupby` of the `Impute` Operation.
#' @param scale (_PositionFieldDef_) An object defining properties of the channel's scale, which is the function that transforms values in the data domain (numbers, dates, strings, etc) to visual values (pixels, colors, sizes) of the encoding channels.
#' 
#' If `null`, the scale will be [disabled and the data value will be directly encoded](https://vega.github.io/vega-lite/docs/scale.html#disable).
#' 
#' __Default value:__ If undefined, default [scale properties](https://vega.github.io/vega-lite/docs/scale.html) are applied.
#' @param sort (_PositionFieldDef_) Sort order for the encoded field.
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
#' __Note:__ `null` is not supported for `row` and `column`.
#' @param stack (_PositionFieldDef_) Type of stacking offset if the field should be stacked.
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
#' (3) At least one of non-position channels mapped to an unaggregated field that is different from x and y.  Otherwise, `null` by default.
#' @param timeUnit (_PositionFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_PositionFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param type (_PositionFieldDef_) The encoded field's type of measurement (`"quantitative"`, `"temporal"`, `"ordinal"`, or `"nominal"`).
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
#' - Secondary channels (e.g., `x2`, `y2`, `xError`, `yError`) do not have `type` as they have exactly the same type as their primary channels (e.g., `x`, `y`).
#' @param value (_YValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_Y <- function(aggregate = NULL, axis = NULL, bin = NULL, field = NULL, impute = NULL, scale = NULL, sort = NULL, stack = NULL, timeUnit = NULL, title = NULL, type = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_Y2
#' 
#' Create spec for Y2.
#' @param aggregate (_SecondaryFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_SecondaryFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_SecondaryFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_SecondaryFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_SecondaryFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param value (_YValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_Y2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_YError
#' 
#' Create spec for YError.
#' @param aggregate (_SecondaryFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_SecondaryFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_SecondaryFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_SecondaryFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_SecondaryFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param value (_NumberValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_YError <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
 #' vl_make_YError2
#' 
#' Create spec for YError2.
#' @param aggregate (_SecondaryFieldDef_) Aggregation function for the field
#' (e.g., `mean`, `sum`, `median`, `min`, `max`, `count`).
#' 
#' __Default value:__ `undefined` (None)
#' @param bin (_SecondaryFieldDef_) A flag for binning a `quantitative` field, [an object defining binning parameters](https://vega.github.io/vega-lite/docs/bin.html#params), or indicating that the data for `x` or `y` channel are binned before they are imported into Vega-Lite (`"binned"`).
#' 
#' - If `true`, default [binning parameters](https://vega.github.io/vega-lite/docs/bin.html) will be applied.
#' 
#' - If `"binned"`, this indicates that the data for the `x` (or `y`) channel are already binned. You can map the bin-start field to `x` (or `y`) and the bin-end field to `x2` (or `y2`). The scale and axis will be formatted similar to binning in Vega-lite.  To adjust the axis ticks based on the bin step, you can also set the axis's [`tickMinStep`](https://vega.github.io/vega-lite/docs/axis.html#ticks) property.
#' 
#' __Default value:__ `false`
#' @param field (_SecondaryFieldDef_) __Required.__ A string defining the name of the field from which to pull a data value
#' or an object defining iterated values from the [`repeat`](https://vega.github.io/vega-lite/docs/repeat.html) operator.
#' 
#' __Note:__ Dots (`.`) and brackets (`[` and `]`) can be used to access nested objects (e.g., `"field": "foo.bar"` and `"field": "foo\['bar'\]"`).
#' If field names contain dots or brackets but are not nested, you can use `\\` to escape dots and brackets (e.g., `"a\\.b"` and `"a\\\[0\\\]"`).
#' See more details about escaping in the [field documentation](https://vega.github.io/vega-lite/docs/field.html).
#' 
#' __Note:__ `field` is not required if `aggregate` is `count`.
#' @param timeUnit (_SecondaryFieldDef_) Time unit (e.g., `year`, `yearmonth`, `month`, `hours`) for a temporal field.
#' or [a temporal field that gets casted as ordinal](https://vega.github.io/vega-lite/docs/type.html#cast).
#' 
#' __Default value:__ `undefined` (None)
#' @param title (_SecondaryFieldDef_) A title for the field. If `null`, the title will be removed.
#' 
#' __Default value:__  derived from the field's name and transformation function (`aggregate`, `bin` and `timeUnit`).  If the field has an aggregate function, the function is displayed as part of the title (e.g., `"Sum of Profit"`). If the field is binned or has a time unit applied, the applied function is shown in parentheses (e.g., `"Profit (binned)"`, `"Transaction Date (year-month)"`).  Otherwise, the title is simply the field name.
#' 
#' __Notes__:
#' 
#' 1) You can customize the default field title format by providing the [`fieldTitle`](https://vega.github.io/vega-lite/docs/config.html#top-level-config) property in the [config](https://vega.github.io/vega-lite/docs/config.html) or [`fieldTitle` function via the `compile` function's options](https://vega.github.io/vega-lite/docs/compile.html#field-title).
#' 
#' 2) If both field definition's `title` and axis, header, or legend `title` are defined, axis/header/legend title will be used.
#' @param value (_NumberValueDef_) A constant value in visual domain (e.g., `"red"` / "#0099ff" for color, values between `0` to `1` for opacity).
#' @return A modified spec
#' @export
vl_make_YError2 <- function(aggregate = NULL, bin = NULL, field = NULL, timeUnit = NULL, title = NULL, value = NULL) {
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}
