% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_autogen_api.R
\name{vl_fold}
\alias{vl_fold}
\title{vl_fold}
\usage{
vl_fold(spec, fold = NULL, as = NULL, .object = NULL)
}
\arguments{
\item{spec}{An input vega-lite spec}

\item{fold}{An array of data fields indicating the properties to fold.}

\item{as}{The output field names for the key and value properties produced by the fold
transform. \strong{Default value:} \verb{["key", "value"]}}

\item{.object}{Directly input an object, rather than creating one via the other arguments. Should not be used in conjunction with the other arguments other than \code{spec}. Objects can be of type: FoldTransform}
}
\value{
A modified Vega-Lite Spec
}
\description{
Add FoldTransform to a vega-lite spec.
}
