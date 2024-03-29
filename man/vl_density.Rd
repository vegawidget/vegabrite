% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_autogen_api.R
\name{vl_density}
\alias{vl_density}
\title{vl_density}
\usage{
vl_density(
  spec,
  density = NULL,
  as = NULL,
  bandwidth = NULL,
  counts = NULL,
  cumulative = NULL,
  extent = NULL,
  groupby = NULL,
  maxsteps = NULL,
  minsteps = NULL,
  steps = NULL,
  .object = NULL
)
}
\arguments{
\item{spec}{An input vega-lite spec}

\item{density}{The data field for which to perform density estimation.}

\item{as}{The output fields for the sample value and corresponding density estimate.

\strong{Default value:} \verb{["value", "density"]}}

\item{bandwidth}{The bandwidth (standard deviation) of the Gaussian kernel. If unspecified or
set to zero, the bandwidth value is automatically estimated from the input data
using Scott’s rule.}

\item{counts}{A boolean flag indicating if the output values should be probability estimates
(false) or smoothed counts (true).

\strong{Default value:} \code{false}}

\item{cumulative}{A boolean flag indicating whether to produce density estimates (false) or
cumulative density estimates (true).

\strong{Default value:} \code{false}}

\item{extent}{A [min, max] domain from which to sample the distribution. If unspecified, the
extent will be determined by the observed minimum and maximum values of the
density value field.}

\item{groupby}{The data fields to group by. If not specified, a single group containing all
data objects will be used.}

\item{maxsteps}{The maximum number of samples to take along the extent domain for plotting the
density.

\strong{Default value:} \code{200}}

\item{minsteps}{The minimum number of samples to take along the extent domain for plotting the
density.

\strong{Default value:} \code{25}}

\item{steps}{The exact number of samples to take along the extent domain for plotting the
density. If specified, overrides both minsteps and maxsteps to set an exact
number of uniform samples. Potentially useful in conjunction with a fixed extent
to ensure consistent sample points for stacked densities.}

\item{.object}{Directly input an object, rather than creating one via the other arguments. Should not be used in conjunction with the other arguments other than \code{spec}. Objects can be of type: DensityTransform}
}
\value{
A modified Vega-Lite Spec
}
\description{
Add DensityTransform to a vega-lite spec.
}
