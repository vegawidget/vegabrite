% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_autogen_api.R
\name{vl_quantile}
\alias{vl_quantile}
\title{vl_quantile}
\usage{
vl_quantile(
  spec,
  quantile = NULL,
  as = NULL,
  groupby = NULL,
  probs = NULL,
  step = NULL,
  .object = NULL
)
}
\arguments{
\item{spec}{An input vega-lite spec}

\item{quantile}{The data field for which to perform quantile estimation.}

\item{as}{The output field names for the probability and quantile values.

\strong{Default value:} \verb{["prob", "value"]}}

\item{groupby}{The data fields to group by. If not specified, a single group containing all
data objects will be used.}

\item{probs}{An array of probabilities in the range (0, 1) for which to compute quantile
values. If not specified, the \emph{step} parameter will be used.}

\item{step}{A probability step size (default 0.01) for sampling quantile values. All values
from one-half the step size up to 1 (exclusive) will be sampled. This parameter
is only used if the \emph{probs} parameter is not provided.}

\item{.object}{Directly input an object, rather than creating one via the other arguments. Should not be used in conjunction with the other arguments other than \code{spec}. Objects can be of type: QuantileTransform}
}
\value{
A modified Vega-Lite Spec
}
\description{
Add QuantileTransform to a vega-lite spec.
}
