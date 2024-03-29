% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_autogen_api.R
\name{vl_encode_yError2}
\alias{vl_encode_yError2}
\title{vl_encode_yError2}
\usage{
vl_encode_yError2(
  spec,
  field = NULL,
  value = NULL,
  aggregate = NULL,
  bandPosition = NULL,
  bin = NULL,
  timeUnit = NULL,
  title = NULL,
  .object = NULL
)
}
\arguments{
\item{spec}{An input vega-lite spec}

\item{field}{\strong{Required.} A string defining the name of the field from which
to pull a data value or an object defining iterated values from the
\href{https://vega.github.io/vega-lite/docs/repeat.html}{\code{repeat}} operator.

\strong{See also:} \href{https://vega.github.io/vega-lite/docs/field.html}{\code{field}}
documentation.

\strong{Notes:} 1) Dots (\code{.}) and brackets (\code{[} and \verb{]}) can be used to access nested
objects (e.g., \code{"field": "foo.bar"} and \verb{"field": "foo\['bar'\]"}). If field names
contain dots or brackets but are not nested, you can use \verb{\\\\} to escape dots and
brackets (e.g., \code{"a\\\\.b"} and \verb{"a\\\\\[0\\\\\]"}). See more details about escaping in
the \href{https://vega.github.io/vega-lite/docs/field.html}{field documentation}. 2)
\code{field} is not required if \code{aggregate} is \code{count}.}

\item{value}{A constant value in visual domain (e.g., \code{"red"} / \code{"#0099ff"} / \href{https://vega.github.io/vega-lite/docs/types.html#gradient}{gradient definition} for
color, values between \code{0} to \code{1} for opacity).}

\item{aggregate}{Aggregation function for the field (e.g., \code{"mean"}, \code{"sum"}, \code{"median"},
\code{"min"}, \code{"max"}, \code{"count"}).

\strong{Default value:} \code{undefined} (None)

\strong{See also:}
\href{https://vega.github.io/vega-lite/docs/aggregate.html}{\code{aggregate}}
documentation.}

\item{bandPosition}{Relative position on a band of a stacked, binned, time unit, or band scale. For
example, the marks will be positioned at the beginning of the band if set to
\code{0}, and at the middle of the band if set to \code{0.5}.}

\item{bin}{A flag for binning a \code{quantitative} field, \href{https://vega.github.io/vega-lite/docs/bin.html#bin-parameters}{an object defining binning parameters},
or indicating that the data for \code{x} or \code{y} channel are binned before they are
imported into Vega-Lite (\code{"binned"}).
\itemize{
\item If \code{true}, default \href{https://vega.github.io/vega-lite/docs/bin.html#bin-parameters}{binning parameters} will
be applied.
\item If \code{"binned"}, this indicates that the data for the \code{x} (or \code{y})
channel are already binned. You can map the bin-start field to \code{x}
(or \code{y}) and the bin-end field to \code{x2} (or \code{y2}). The scale and
axis will be formatted similar to binning in Vega-Lite. To adjust
the axis ticks based on the bin step, you can also set the axis's
\href{https://vega.github.io/vega-lite/docs/axis.html#ticks}{\code{tickMinStep}} property.
}

\strong{Default value:} \code{false}

\strong{See also:} \href{https://vega.github.io/vega-lite/docs/bin.html}{\code{bin}}
documentation.}

\item{timeUnit}{Time unit (e.g., \code{year}, \code{yearmonth}, \code{month}, \code{hours}) for
a temporal field. or \href{https://vega.github.io/vega-lite/docs/type.html#cast}{a temporal field that gets casted as ordinal}.

\strong{Default value:} \code{undefined} (None)

\strong{See also:} \href{https://vega.github.io/vega-lite/docs/timeunit.html}{\code{timeUnit}}
documentation.}

\item{title}{A title for the field. If \code{null}, the title will be removed.

\strong{Default value:} derived from the field's name and transformation function
(\code{aggregate}, \code{bin} and \code{timeUnit}). If the field has an aggregate function,
the function is displayed as part of the title (e.g., \code{"Sum of Profit"}). If
the field is binned or has a time unit applied, the applied function is shown
in parentheses (e.g., \code{"Profit (binned)"}, \code{"Transaction Date (year-month)"}).
Otherwise, the title is simply the field name.

\strong{Notes}:
\enumerate{
\item You can customize the default field title format by providing the
\href{https://vega.github.io/vega-lite/docs/config.html#top-level-config}{\code{fieldTitle}}
property in the \href{https://vega.github.io/vega-lite/docs/config.html}{config}
or \href{https://vega.github.io/vega-lite/usage/compile.html#field-title}{\code{fieldTitle} function via the \code{compile} function's options}.
\item If both field definition's \code{title} and axis, header, or legend \code{title} are
defined, axis/header/legend title will be used.
}}

\item{.object}{Directly input an object, rather than creating one via the other arguments. Should not be used in conjunction with the other arguments other than \code{spec}. Objects can be of type: SecondaryFieldDef or ValueDef<number>}
}
\value{
A modified Vega-Lite Spec
}
\description{
Add encoding for yError2 to a vega-lite spec.
}
