% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_autogen_api.R
\name{vl_mark_errorbar}
\alias{vl_mark_errorbar}
\title{vl_mark_errorbar}
\usage{
vl_mark_errorbar(
  spec,
  clip = NULL,
  color = NULL,
  extent = NULL,
  opacity = NULL,
  orient = NULL,
  rule = NULL,
  size = NULL,
  thickness = NULL,
  ticks = NULL,
  .object = NULL
)
}
\arguments{
\item{spec}{An input vega-lite spec}

\item{clip}{Whether a composite mark be clipped to the enclosing group’s width and height.}

\item{color}{Default color.

\strong{Default value:} <span style="color: #4682b4;">■</span> \code{"#4682b4"}

\strong{Note:}
\itemize{
\item This property cannot be used in a \href{https://vega.github.io/vega-lite/docs/mark.html#style-config}{style config}.
\item The \code{fill} and \code{stroke} properties have higher precedence than \code{color} and
will override \code{color}.
}}

\item{extent}{The extent of the rule. Available options include:
\itemize{
\item \code{"ci"}: Extend the rule to the confidence interval of the mean.
\item \code{"stderr"}: The size of rule are set to the value of standard error, extending
from the mean.
\item \code{"stdev"}: The size of rule are set to the value of standard deviation,
extending from the mean.
\item \code{"iqr"}: Extend the rule to the q1 and q3.
}

\strong{Default value:} \code{"stderr"}.}

\item{opacity}{The opacity (value between [0,1]) of the mark.}

\item{orient}{Orientation of the error bar. This is normally automatically determined, but
can be specified when the orientation is ambiguous and cannot be automatically
determined.}

\item{rule}{boolean

Or: MarkConfig}

\item{size}{Size of the ticks of an error bar}

\item{thickness}{Thickness of the ticks and the bar of an error bar}

\item{ticks}{boolean

Or: MarkConfig}

\item{.object}{Directly input an object, rather than creating one via the other arguments. Should not be used in conjunction with the other arguments other than \code{spec}. Objects can be of type: ErrorBarDef}
}
\value{
A modified Vega-Lite Spec
}
\description{
vl_mark_errorbar
}
