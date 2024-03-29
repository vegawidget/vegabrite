% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_autogen_api.R
\name{vl_config_line}
\alias{vl_config_line}
\title{vl_config_line}
\usage{
vl_config_line(
  spec,
  align = NULL,
  angle = NULL,
  aria = NULL,
  ariaRole = NULL,
  ariaRoleDescription = NULL,
  aspect = NULL,
  baseline = NULL,
  blend = NULL,
  color = NULL,
  cornerRadius = NULL,
  cornerRadiusBottomLeft = NULL,
  cornerRadiusBottomRight = NULL,
  cornerRadiusTopLeft = NULL,
  cornerRadiusTopRight = NULL,
  cursor = NULL,
  description = NULL,
  dir = NULL,
  dx = NULL,
  dy = NULL,
  ellipsis = NULL,
  endAngle = NULL,
  fill = NULL,
  fillOpacity = NULL,
  filled = NULL,
  font = NULL,
  fontSize = NULL,
  fontStyle = NULL,
  fontWeight = NULL,
  height = NULL,
  href = NULL,
  innerRadius = NULL,
  interpolate = NULL,
  invalid = NULL,
  limit = NULL,
  lineBreak = NULL,
  lineHeight = NULL,
  opacity = NULL,
  order = NULL,
  orient = NULL,
  outerRadius = NULL,
  padAngle = NULL,
  point = NULL,
  radius = NULL,
  radius2 = NULL,
  shape = NULL,
  size = NULL,
  smooth = NULL,
  startAngle = NULL,
  stroke = NULL,
  strokeCap = NULL,
  strokeDash = NULL,
  strokeDashOffset = NULL,
  strokeJoin = NULL,
  strokeMiterLimit = NULL,
  strokeOffset = NULL,
  strokeOpacity = NULL,
  strokeWidth = NULL,
  tension = NULL,
  text = NULL,
  theta = NULL,
  theta2 = NULL,
  timeUnitBandPosition = NULL,
  timeUnitBandSize = NULL,
  tooltip = NULL,
  url = NULL,
  width = NULL,
  x = NULL,
  x2 = NULL,
  y = NULL,
  y2 = NULL,
  .object = NULL
)
}
\arguments{
\item{spec}{An input vega-lite spec}

\item{align}{The horizontal alignment of the text or ranged marks (area, bar, image, rect,
rule). One of \code{"left"}, \code{"right"}, \code{"center"}.

\strong{Note:} Expression reference is \emph{not} supported for range marks.}

\item{angle}{The rotation angle of the text, in degrees.

Or: ExprRef}

\item{aria}{A boolean flag indicating if \href{https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA}{ARIA attributes}
should be included (SVG output only). If \code{false}, the "aria-hidden" attribute
will be set on the output SVG element, removing the mark item from the ARIA
accessibility tree.

Or: ExprRef}

\item{ariaRole}{Sets the type of user interface element of the mark item for \href{https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA}{ARIA accessibility}
(SVG output only). If specified, this property determines the "role" attribute.
Warning: this property is experimental and may be changed in the future.

Or: ExprRef}

\item{ariaRoleDescription}{A human-readable, author-localized description
for the role of the mark item for \href{https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA}{ARIA accessibility}
(SVG output only). If specified, this property determines the
"aria-roledescription" attribute. Warning: this property is experimental and may
be changed in the future.

Or: ExprRef}

\item{aspect}{Whether to keep aspect ratio of image marks.

Or: ExprRef}

\item{baseline}{For text marks, the vertical text baseline. One of \code{"alphabetic"} (default),
\code{"top"}, \code{"middle"}, \code{"bottom"}, \code{"line-top"}, \code{"line-bottom"}, or an
expression reference that provides one of the valid values. The \code{"line-top"}
and \code{"line-bottom"} values operate similarly to \code{"top"} and \code{"bottom"}, but are
calculated relative to the \code{lineHeight} rather than \code{fontSize} alone.

For range marks, the vertical alignment of the marks. One of \code{"top"},
\code{"middle"}, \code{"bottom"}.

\strong{Note:} Expression reference is \emph{not} supported for range marks.}

\item{blend}{The color blend mode for drawing an item
on its current background. Any valid \href{https://developer.mozilla.org/en-US/docs/Web/CSS/mix-blend-mode}{CSS mix-blend-mode}
value can be used.

__Default value: \code{"source-over"}

Or: ExprRef}

\item{color}{Default color.

\strong{Default value:} <span style="color: #4682b4;">■</span> \code{"#4682b4"}

\strong{Note:}
\itemize{
\item This property cannot be used in a \href{https://vega.github.io/vega-lite/docs/mark.html#style-config}{style config}.
\item The \code{fill} and \code{stroke} properties have higher precedence than \code{color} and
will override \code{color}.
}}

\item{cornerRadius}{The radius in pixels of rounded rectangles or arcs' corners.

\strong{Default value:} \code{0}

Or: ExprRef}

\item{cornerRadiusBottomLeft}{The radius in pixels of rounded rectangles' bottom left corner.

\strong{Default value:} \code{0}

Or: ExprRef}

\item{cornerRadiusBottomRight}{The radius in pixels of rounded rectangles' bottom right corner.

\strong{Default value:} \code{0}

Or: ExprRef}

\item{cornerRadiusTopLeft}{The radius in pixels of rounded rectangles' top right corner.

\strong{Default value:} \code{0}

Or: ExprRef}

\item{cornerRadiusTopRight}{The radius in pixels of rounded rectangles' top left corner.

\strong{Default value:} \code{0}

Or: ExprRef}

\item{cursor}{The mouse cursor used over the mark. Any valid \href{https://developer.mozilla.org/en-US/docs/Web/CSS/cursor#Values}{CSS cursor type} can be
used.

Or: ExprRef}

\item{description}{A text description of the mark item for \href{https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA}{ARIA accessibility}
(SVG output only). If specified, this property determines the \href{https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_aria-label_attribute}{"aria-label" attribute}.

Or: ExprRef}

\item{dir}{The direction of the text. One of \code{"ltr"} (left-to-right) or \code{"rtl"}
(right-to-left). This property determines on which side is truncated in response
to the limit parameter.

\strong{Default value:} \code{"ltr"}

Or: ExprRef}

\item{dx}{The horizontal offset, in pixels, between the text label and its anchor point.
The offset is applied after rotation by the \emph{angle} property.

Or: ExprRef}

\item{dy}{The vertical offset, in pixels, between the text label and its anchor point. The
offset is applied after rotation by the \emph{angle} property.

Or: ExprRef}

\item{ellipsis}{The ellipsis string for text truncated in response to the limit parameter.

\strong{Default value:} \code{"…"}

Or: ExprRef}

\item{endAngle}{The end angle in radians for arc marks. A value of \code{0} indicates up (north),
increasing values proceed clockwise.

Or: ExprRef}

\item{fill}{Default fill color. This property has higher precedence than \code{config.color}. Set
to \code{null} to remove fill.

\strong{Default value:} (None)}

\item{fillOpacity}{The fill opacity (value between [0,1]).

\strong{Default value:} \code{1}

Or: ExprRef}

\item{filled}{Whether the mark's color should be used as fill color instead of stroke color.

\strong{Default value:} \code{false} for all \code{point}, \code{line},
and \code{rule} marks as well as \code{geoshape} marks for
\href{https://vega.github.io/vega-lite/docs/data.html#graticule}{\code{graticule}} data
sources; otherwise, \code{true}.

\strong{Note:} This property cannot be used in a \href{https://vega.github.io/vega-lite/docs/mark.html#style-config}{style config}.}

\item{font}{The typeface to set the text in (e.g., \code{"Helvetica Neue"}).

Or: ExprRef}

\item{fontSize}{The font size, in pixels.

\strong{Default value:} \code{11}

Or: ExprRef}

\item{fontStyle}{The font style (e.g., \code{"italic"}).

Or: ExprRef}

\item{fontWeight}{The font weight. This can be either a string (e.g \code{"bold"}, \code{"normal"}) or a
number (\code{100}, \code{200}, \code{300}, ..., \code{900} where \code{"normal"} = \code{400} and \code{"bold"} =
\code{700}).

Or: ExprRef}

\item{height}{Height of the marks.

Or: ExprRef}

\item{href}{A URL to load upon mouse click. If defined, the mark acts as a hyperlink.

Or: ExprRef}

\item{innerRadius}{The inner radius in pixels of arc marks. \code{innerRadius} is an alias for
\code{radius2}.

\strong{Default value:} \code{0}}

\item{interpolate}{The line interpolation method to use for line and area marks. One of the
following:
\itemize{
\item \code{"linear"}: piecewise linear segments, as in a polyline.
\item \code{"linear-closed"}: close the linear segments to form a polygon.
\item \code{"step"}: alternate between horizontal and vertical segments, as in a step
function.
\item \code{"step-before"}: alternate between vertical and horizontal segments, as in a
step function.
\item \code{"step-after"}: alternate between horizontal and vertical segments, as in a
step function.
\item \code{"basis"}: a B-spline, with control point duplication on the ends.
\item \code{"basis-open"}: an open B-spline; may not intersect the start or end.
\item \code{"basis-closed"}: a closed B-spline, as in a loop.
\item \code{"cardinal"}: a Cardinal spline, with control point duplication on the ends.
\item \code{"cardinal-open"}: an open Cardinal spline; may not intersect the start or
end, but will intersect other control points.
\item \code{"cardinal-closed"}: a closed Cardinal spline, as in a loop.
\item \code{"bundle"}: equivalent to basis, except the tension parameter is used to
straighten the spline.
\item \code{"monotone"}: cubic interpolation that preserves monotonicity in y.
}

Or: ExprRef}

\item{invalid}{Defines how Vega-Lite should handle marks for invalid values (\code{null} and \code{NaN}).
\itemize{
\item If set to \code{"filter"} (default), all data items with null values will be
skipped (for line, trail, and area marks) or filtered (for other marks).
\item If \code{null}, all data items are included. In this case, invalid values will be
interpreted as zeroes.
}}

\item{limit}{The maximum length of the text mark in pixels. The text value will be
automatically truncated if the rendered size exceeds the limit.

\strong{Default value:} \code{0} -- indicating no limit

Or: ExprRef}

\item{lineBreak}{A delimiter, such as a newline character, upon which to break text strings into
multiple lines. This property is ignored if the text is array-valued.

Or: ExprRef}

\item{lineHeight}{The line height in pixels (the spacing between subsequent lines of text) for
multi-line text marks.

Or: ExprRef}

\item{opacity}{The overall opacity (value between [0,1]).

\strong{Default value:} \code{0.7} for non-aggregate plots with \code{point}, \code{tick}, \code{circle},
or \code{square} marks or layered \code{bar} charts and \code{1} otherwise.}

\item{order}{For line and trail marks, this \code{order} property can be set to \code{null} or \code{false}
to make the lines use the original order in the data sources.}

\item{orient}{The orientation of a non-stacked bar, tick, area, and line charts. The value is
either horizontal (default) or vertical.
\itemize{
\item For bar, rule and tick, this determines whether the size of the bar and tick
should be applied to x or y dimension.
\item For area, this property determines the orient property of the Vega output.
\item For line and trail marks, this property determines the sort order of the
points in the line if \code{config.sortLineBy} is not specified. For stacked charts,
this is always determined by the orientation of the stack; therefore explicitly
specified value will be ignored.
}}

\item{outerRadius}{The outer radius in pixels of arc marks. \code{outerRadius} is an alias for \code{radius}.

\strong{Default value:} \code{0}}

\item{padAngle}{The angular padding applied to sides of the arc, in radians.

Or: ExprRef}

\item{point}{A flag for overlaying points on top of line or area marks, or an object defining
the properties of the overlayed points.
\itemize{
\item If this property is \code{"transparent"}, transparent points will be used (for
enhancing tooltips and selections).
\item If this property is an empty object (\code{{}}) or \code{true}, filled points with
default properties will be used.
\item If this property is \code{false}, no points would be automatically added to line or
area marks.
}

\strong{Default value:} \code{false}.}

\item{radius}{For arc mark, the primary (outer) radius in pixels.

For text marks, polar coordinate radial offset, in pixels, of the text from the
origin determined by the \code{x} and \code{y} properties.

\strong{Default value:} \code{min(plot_width, plot_height)/2}}

\item{radius2}{The secondary (inner) radius in pixels of arc marks.

\strong{Default value:} \code{0}}

\item{shape}{Shape of the point marks. Supported values include:
\itemize{
\item plotting shapes: \code{"circle"}, \code{"square"}, \code{"cross"}, \code{"diamond"},
\code{"triangle-up"}, \code{"triangle-down"}, \code{"triangle-right"}, or \code{"triangle-left"}.
\item the line symbol \code{"stroke"}
\item centered directional shapes \code{"arrow"}, \code{"wedge"}, or \code{"triangle"}
\item a custom \href{https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths}{SVG path string} (For
correct sizing, custom shape paths should be defined within a square bounding
box with coordinates ranging from -1 to 1 along both the x and y dimensions.)
}

\strong{Default value:} \code{"circle"}

Or: ExprRef}

\item{size}{Default size for marks.
\itemize{
\item For \code{point}/\code{circle}/\code{square}, this represents the pixel area of the marks.
Note that this value sets the area of the symbol; the side lengths will increase
with the square root of this value.
\item For \code{bar}, this represents the band size of the bar, in pixels.
\item For \code{text}, this represents the font size, in pixels.
}

\strong{Default value:}
\itemize{
\item \code{30} for point, circle, square marks; width/height's \code{step}
\item \code{2} for bar marks with discrete dimensions;
\item \code{5} for bar marks with continuous dimensions;
\item \code{11} for text marks.
}}

\item{smooth}{A boolean flag (default true) indicating if the image should be smoothed when
resized. If false, individual pixels should be scaled directly rather than
interpolated with smoothing. For SVG rendering, this option may not work in some
browsers due to lack of standardization.

Or: ExprRef}

\item{startAngle}{The start angle in radians for arc marks. A value of \code{0} indicates up (north),
increasing values proceed clockwise.

Or: ExprRef}

\item{stroke}{Default stroke color. This property has higher precedence than \code{config.color}.
Set to \code{null} to remove stroke.

\strong{Default value:} (None)}

\item{strokeCap}{The stroke cap for line ending style. One of \code{"butt"}, \code{"round"}, or \code{"square"}.

\strong{Default value:} \code{"butt"}

Or: ExprRef}

\item{strokeDash}{An array of alternating stroke, space lengths for creating dashed or dotted
lines.

Or: ExprRef}

\item{strokeDashOffset}{The offset (in pixels) into which to begin drawing with the stroke dash array.

Or: ExprRef}

\item{strokeJoin}{The stroke line join method. One of \code{"miter"}, \code{"round"} or \code{"bevel"}.

\strong{Default value:} \code{"miter"}

Or: ExprRef}

\item{strokeMiterLimit}{The miter limit at which to bevel a line join.

Or: ExprRef}

\item{strokeOffset}{The offset in pixels at which to draw the group stroke and fill. If unspecified,
the default behavior is to dynamically offset stroked groups such that 1 pixel
stroke widths align with the pixel grid.

Or: ExprRef}

\item{strokeOpacity}{The stroke opacity (value between [0,1]).

\strong{Default value:} \code{1}

Or: ExprRef}

\item{strokeWidth}{The stroke width, in pixels.

Or: ExprRef}

\item{tension}{Depending on the interpolation type, sets the tension parameter (for line and
area marks).

Or: ExprRef}

\item{text}{Placeholder text if the \code{text} channel is not specified

Or: ExprRef}

\item{theta}{\itemize{
\item For arc marks, the arc length in radians if theta2 is not specified, otherwise
the start arc angle. (A value of 0 indicates up or “north”, increasing values
proceed clockwise.)
\item For text marks, polar coordinate angle in radians.
}}

\item{theta2}{The end angle of arc marks in radians. A value of 0 indicates up or “north”,
increasing values proceed clockwise.}

\item{timeUnitBandPosition}{Default relative band position for a time unit. If set to \code{0}, the marks will
be positioned at the beginning of the time unit band step. If set to \code{0.5}, the
marks will be positioned in the middle of the time unit band step.}

\item{timeUnitBandSize}{Default relative band size for a time unit. If set to \code{1}, the bandwidth of the
marks will be equal to the time unit band step. If set to \code{0.5}, bandwidth of
the marks will be half of the time unit band step.}

\item{tooltip}{The tooltip text string to show upon mouse hover or an object defining which
fields should the tooltip be derived from.
\itemize{
\item If \code{tooltip} is \code{true} or \code{{"content": "encoding"}}, then all fields from
\code{encoding} will be used.
\item If \code{tooltip} is \code{{"content": "data"}}, then all fields that appear in the
highlighted data point will be used.
\item If set to \code{null} or \code{false}, then no tooltip will be used.
}

See the \href{https://vega.github.io/vega-lite/docs/tooltip.html}{\code{tooltip}}
documentation for a detailed discussion about tooltip in Vega-Lite.

\strong{Default value:} \code{null}}

\item{url}{The URL of the image file for image marks.

Or: ExprRef}

\item{width}{Width of the marks.

Or: ExprRef}

\item{x}{X coordinates of the marks, or width of horizontal \code{"bar"} and \code{"area"} without
specified \code{x2} or \code{width}.

The \code{value} of this channel can be a number or a string \code{"width"} for the width
of the plot.}

\item{x2}{X2 coordinates for ranged \code{"area"}, \code{"bar"}, \code{"rect"}, and \code{"rule"}.

The \code{value} of this channel can be a number or a string \code{"width"} for the width
of the plot.}

\item{y}{Y coordinates of the marks, or height of vertical \code{"bar"} and \code{"area"} without
specified \code{y2} or \code{height}.

The \code{value} of this channel can be a number or a string \code{"height"} for the
height of the plot.}

\item{y2}{Y2 coordinates for ranged \code{"area"}, \code{"bar"}, \code{"rect"}, and \code{"rule"}.

The \code{value} of this channel can be a number or a string \code{"height"} for the
height of the plot.}

\item{.object}{Directly input an object, rather than creating one via the other arguments. Should not be used in conjunction with the other arguments other than \code{spec}. Objects can be of type: LineConfig}
}
\value{
A modified Vega-Lite Spec
}
\description{
Add line config (LineConfig) to a vega-lite spec.
}
