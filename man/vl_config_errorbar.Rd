% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_autogen_api.R
\name{vl_config_errorbar}
\alias{vl_config_errorbar}
\title{vl_config_errorbar}
\usage{
vl_config_errorbar(
  spec,
  extent = NULL,
  rule = NULL,
  size = NULL,
  thickness = NULL,
  ticks = NULL,
  .object = NULL
)
}
\arguments{
\item{spec}{An input vega-lite spec}

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

\item{rule}{boolean

Or: MarkConfig}

\item{size}{Size of the ticks of an error bar}

\item{thickness}{Thickness of the ticks and the bar of an error bar}

\item{ticks}{boolean

Or: MarkConfig}

\item{.object}{Directly input an object, rather than creating one via the other arguments. Should not be used in conjunction with the other arguments other than \code{spec}. Objects can be of type: ErrorBarConfig}
}
\value{
A modified Vega-Lite Spec
}
\description{
Add errorbar config (ErrorBarConfig) to a vega-lite spec.
}
