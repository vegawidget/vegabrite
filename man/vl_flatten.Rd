% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_autogen_api.R
\name{vl_flatten}
\alias{vl_flatten}
\title{vl_flatten}
\usage{
vl_flatten(spec, flatten = NULL, as = NULL, .object = NULL)
}
\arguments{
\item{spec}{An input vega-lite spec}

\item{flatten}{An array of one or more data fields containing arrays to flatten. If multiple
fields are specified, their array values should have a parallel structure,
ideally with the same length. If the lengths of parallel arrays do not match,
the longest array will be used with \code{null} values added for missing entries.}

\item{as}{The output field names for extracted array values.

\strong{Default value:} The field name of the corresponding array field}

\item{.object}{Directly input an object, rather than creating one via the other arguments. Should not be used in conjunction with the other arguments other than \code{spec}. Objects can be of type: FlattenTransform}
}
\value{
A modified Vega-Lite Spec
}
\description{
Add FlattenTransform to a vega-lite spec.
}
