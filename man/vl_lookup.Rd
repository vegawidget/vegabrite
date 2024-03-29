% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_autogen_api.R
\name{vl_lookup}
\alias{vl_lookup}
\title{vl_lookup}
\usage{
vl_lookup(
  spec,
  lookup = NULL,
  from = NULL,
  as = NULL,
  default = NULL,
  .object = NULL
)
}
\arguments{
\item{spec}{An input vega-lite spec}

\item{lookup}{Key in primary data source.}

\item{from}{Data source or selection for secondary data reference.}

\item{as}{The output fields on which to store the looked up data values.

For data lookups, this property may be left blank if \code{from.fields} has been
specified (those field names will be used); if \code{from.fields} has not been
specified, \code{as} must be a string.

For selection lookups, this property is optional: if unspecified, looked
up values will be stored under a property named for the selection; and if
specified, it must correspond to \code{from.fields}.}

\item{default}{The default value to use if lookup fails.

\strong{Default value:} \code{null}}

\item{.object}{Directly input an object, rather than creating one via the other arguments. Should not be used in conjunction with the other arguments other than \code{spec}. Objects can be of type: LookupTransform}
}
\value{
A modified Vega-Lite Spec
}
\description{
Add LookupTransform to a vega-lite spec.
}
