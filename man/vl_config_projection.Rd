% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_autogen_api.R
\name{vl_config_projection}
\alias{vl_config_projection}
\title{vl_config_projection}
\usage{
vl_config_projection(
  spec,
  center = NULL,
  clipAngle = NULL,
  clipExtent = NULL,
  coefficient = NULL,
  distance = NULL,
  extent = NULL,
  fit = NULL,
  fraction = NULL,
  lobes = NULL,
  parallel = NULL,
  parallels = NULL,
  pointRadius = NULL,
  precision = NULL,
  radius = NULL,
  ratio = NULL,
  reflectX = NULL,
  reflectY = NULL,
  rotate = NULL,
  scale = NULL,
  size = NULL,
  spacing = NULL,
  tilt = NULL,
  translate = NULL,
  type = NULL,
  .object = NULL
)
}
\arguments{
\item{spec}{An input vega-lite spec}

\item{center}{The projection's center, a two-element array of longitude and latitude in
degrees.

\strong{Default value:} \verb{[0, 0]}

Or: ExprRef}

\item{clipAngle}{The projection's clipping circle radius to the specified angle in degrees. If
\code{null}, switches to \href{http://bl.ocks.org/mbostock/3788999}{antimeridian} cutting
rather than small-circle clipping.

Or: ExprRef}

\item{clipExtent}{The projection's viewport clip extent to the specified bounds in pixels. The
extent bounds are specified as an array \verb{[\[x0, y0\], \[x1, y1\]]}, where \code{x0} is
the left-side of the viewport, \code{y0} is the top, \code{x1} is the right and \code{y1} is
the bottom. If \code{null}, no viewport clipping is performed.

Or: ExprRef}

\item{coefficient}{The coefficient parameter for the \code{hammer} projection.

\strong{Default value:} \code{2}

Or: ExprRef}

\item{distance}{For the \code{satellite} projection, the distance from the center of the sphere
to the point of view, as a proportion of the sphere’s radius. The recommended
maximum clip angle for a given \code{distance} is acos(1 / distance) converted
to degrees. If tilt is also applied, then more conservative clipping may be
necessary.

\strong{Default value:} \code{2.0}

Or: ExprRef}

\item{extent}{Vector2<Vector2<number>>

Or: ExprRef}

\item{fit}{array

Or: ExprRef}

\item{fraction}{The fraction parameter for the \code{bottomley} projection.

\strong{Default value:} \code{0.5}, corresponding to a sin(ψ) where ψ = π/6.

Or: ExprRef}

\item{lobes}{The number of lobes in projections that support multi-lobe views: \code{berghaus},
\code{gingery}, or \code{healpix}. The default value varies based on the projection type.

Or: ExprRef}

\item{parallel}{The parallel parameter for projections that support it: \code{armadillo},
\code{bonne}, \code{craig}, \code{cylindricalEqualArea}, \code{cylindricalStereographic},
\code{hammerRetroazimuthal}, \code{loximuthal}, or \code{rectangularPolyconic}. The default
value varies based on the projection type.

Or: ExprRef}

\item{parallels}{For conic projections, the \href{https://en.wikipedia.org/wiki/Map_projection#Conic}{two standard parallels} that define the
map layout. The default depends on the specific conic projection used.

Or: ExprRef}

\item{pointRadius}{The default radius (in pixels) to use when drawing GeoJSON \code{Point} and
\code{MultiPoint} geometries. This parameter sets a constant default value. To modify
the point radius in response to data, see the corresponding parameter of the
GeoPath and GeoShape transforms.

\strong{Default value:} \code{4.5}

Or: ExprRef}

\item{precision}{The threshold for the projection's \href{http://bl.ocks.org/mbostock/3795544}{adaptive resampling} to the specified
value in pixels. This value corresponds to the \href{http://en.wikipedia.org/wiki/Ramer-Douglas-Peucker_algorithm}{Douglas–Peucker distance}.
If precision is not specified, returns the projection's current resampling
precision which defaults to \verb{√0.5 ≅ 0.70710…}.

Or: ExprRef}

\item{radius}{The radius parameter for the \code{airy} or \code{gingery} projection. The default value
varies based on the projection type.

Or: ExprRef}

\item{ratio}{The ratio parameter for the \code{hill}, \code{hufnagel}, or \code{wagner} projections. The
default value varies based on the projection type.

Or: ExprRef}

\item{reflectX}{Sets whether or not the x-dimension is reflected (negated) in the output.

Or: ExprRef}

\item{reflectY}{Sets whether or not the y-dimension is reflected (negated) in the output.

Or: ExprRef}

\item{rotate}{The projection's three-axis rotation to the specified angles, which must be a
two- or three-element array of numbers [\code{lambda}, \code{phi}, \code{gamma}] specifying the
rotation angles in degrees about each spherical axis. (These correspond to yaw,
pitch and roll.)

\strong{Default value:} \verb{[0, 0, 0]}

Or: ExprRef}

\item{scale}{The projection’s scale (zoom) factor, overriding automatic fitting. The
default scale is projection-specific. The scale factor corresponds linearly
to the distance between projected points; however, scale factor values are not
equivalent across projections.}

\item{size}{Used in conjunction with fit, provides the width and height in pixels of the
area to which the projection should be automatically fit.

Or: ExprRef}

\item{spacing}{The spacing parameter for the \code{lagrange} projection.

\strong{Default value:} \code{0.5}

Or: ExprRef}

\item{tilt}{The tilt angle (in degrees) for the \code{satellite} projection.

\strong{Default value:} \code{0}.

Or: ExprRef}

\item{translate}{The projection’s translation offset as a two-element array \verb{[tx, ty]}.}

\item{type}{The cartographic projection to use. This value is case-insensitive,
for example \code{"albers"} and \code{"Albers"} indicate the same
projection type. You can find all valid projection types \href{https://vega.github.io/vega-lite/docs/projection.html#projection-types}{in the documentation}.

\strong{Default value:} \code{equalEarth}}

\item{.object}{Directly input an object, rather than creating one via the other arguments. Should not be used in conjunction with the other arguments other than \code{spec}. Objects can be of type: Projection}
}
\value{
A modified Vega-Lite Spec
}
\description{
Add projection config (ProjectionConfig) to a vega-lite spec.
}
