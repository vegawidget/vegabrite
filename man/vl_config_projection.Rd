% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_autogen_api.R
\name{vl_config_projection}
\alias{vl_config_projection}
\title{vl_config_projection}
\usage{
vl_config_projection(
  spec,
  .object = NULL,
  center = NULL,
  clipAngle = NULL,
  clipExtent = NULL,
  coefficient = NULL,
  distance = NULL,
  fraction = NULL,
  lobes = NULL,
  parallel = NULL,
  parallels = NULL,
  precision = NULL,
  radius = NULL,
  ratio = NULL,
  reflectX = NULL,
  reflectY = NULL,
  rotate = NULL,
  scale = NULL,
  spacing = NULL,
  tilt = NULL,
  translate = NULL,
  type = NULL
)
}
\arguments{
\item{spec}{An input vega-lite spec}

\item{.object}{Directly input an object, rather than creating one via the other arguments. Should not be used in conjunction with the other arguments other than 'spec'. Objects can be of type: Projection}

\item{center}{(\emph{Projection}) The projection’s center to the specified center, a two-element array of longitude and latitude in degrees.

\strong{Default value:} \verb{[0, 0]}}

\item{clipAngle}{(\emph{Projection}) The projection’s clipping circle radius to the specified angle in degrees. If \code{null}, switches to \href{http://bl.ocks.org/mbostock/3788999}{antimeridian} cutting rather than small-circle clipping.}

\item{clipExtent}{(\emph{Projection}) The projection’s viewport clip extent to the specified bounds in pixels. The extent bounds are specified as an array \verb{[\[x0, y0\], \[x1, y1\]]}, where \code{x0} is the left-side of the viewport, \code{y0} is the top, \code{x1} is the right and \code{y1} is the bottom. If \code{null}, no viewport clipping is performed.}

\item{coefficient}{(\emph{Projection})}

\item{distance}{(\emph{Projection})}

\item{fraction}{(\emph{Projection})}

\item{lobes}{(\emph{Projection})}

\item{parallel}{(\emph{Projection})}

\item{parallels}{(\emph{Projection})}

\item{precision}{(\emph{Projection}) The threshold for the projection’s \href{http://bl.ocks.org/mbostock/3795544}{adaptive resampling} to the specified value in pixels. This value corresponds to the \href{http://en.wikipedia.org/wiki/Ramer-Douglas-Peucker_algorithm}{Douglas–Peucker distance}. If precision is not specified, returns the projection’s current resampling precision which defaults to \verb{√0.5 ≅ 0.70710…}.}

\item{radius}{(\emph{Projection})}

\item{ratio}{(\emph{Projection})}

\item{reflectX}{(\emph{Projection})}

\item{reflectY}{(\emph{Projection})}

\item{rotate}{(\emph{Projection}) The projection’s three-axis rotation to the specified angles, which must be a two- or three-element array of numbers [\code{lambda}, \code{phi}, \code{gamma}] specifying the rotation angles in degrees about each spherical axis. (These correspond to yaw, pitch and roll.)

\strong{Default value:} \verb{[0, 0, 0]}}

\item{scale}{(\emph{Projection}) The projection's scale (zoom) value, overriding automatic fitting.}

\item{spacing}{(\emph{Projection})}

\item{tilt}{(\emph{Projection})}

\item{translate}{(\emph{Projection}) The projection's translation (pan) value, overriding automatic fitting.}

\item{type}{(\emph{Projection}) The cartographic projection to use. This value is case-insensitive, for example \code{"albers"} and \code{"Albers"} indicate the same projection type. You can find all valid projection types \href{https://vega.github.io/vega-lite/docs/projection.html#projection-types}{in the documentation}.

\strong{Default value:} \code{mercator}}
}
\value{
A modified Vega-Lite Spec
}
\description{
Add projection config (ProjectionConfig) to a vega-lite spec.
}