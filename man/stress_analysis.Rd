% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/various.R
\name{stress_analysis}
\alias{stress_analysis}
\title{Quick analysis of a stress data set}
\usage{
stress_analysis(
  x,
  PoR,
  type = c("none", "in", "out", "right", "left"),
  pb,
  plot = TRUE,
  ...
)
}
\arguments{
\item{x}{\code{data.frame} or \code{sf} object containing the coordinates of the point(s)
(\code{lat}, \code{lon}), the direction of
\eqn{\sigma_{Hmax}}{SHmax} \code{azi} and its standard deviation
\code{unc} (optional)}

\item{PoR}{Pole of Rotation. \code{data.frame} or object of class \code{"euler.pole"}
containing the geographical coordinates of the Euler pole}

\item{type}{Character. Type of plate boundary (optional). Can be
\code{"out"}, \code{"in"}, \code{"right"}, or
\code{"left"} for outward, inward, right-lateral, or left-lateral
moving plate boundaries, respectively. If \code{"none"} (the default), only
the PoR-equivalent azimuth is returned.}

\item{pb}{(optional) \code{sf} object of the plate boundary geometries in the
geographical coordinate system}

\item{plot}{(logical). Whether to produce a plot additional to output.}

\item{...}{optional arguments to \code{\link[=distance_from_pb]{distance_from_pb()}}}
}
\value{
list containing the following values:
\describe{
\item{\code{results}}{data.frame showing the the coordinate and azimuth conversions
(\code{lat.PoR}, \code{lon.PoR}, and \code{azi.PoR}), the predicted azimuths (\code{prd}),
deviation angle from predicted (\code{dev}), circular distance (\code{cdist}),
misfit to predicted stress direction (\code{nchisq}) and, if given, distance to tested
plate boundary (\code{distance})}
\item{\code{stats}}{array with circular (weighted) mean, circular standard
deviation, circular variance, circular median, skewness, kurtosis, the 95\%
confidence angle, circular dispersion, and the normalized Chi-squared test
statistic}
\item{\code{test}}{list containing the test results of the (weighted) Rayleigh
test against the uniform distribution about the predicted orientation.}
}
}
\description{
Returns the converted azimuths, distances to the plate boundary,
statistics of the model, and some plots.
}
\examples{
\donttest{
data("nuvel1")
na_pa <- subset(nuvel1, nuvel1$plate.rot == "na")

data("plates")
plate_boundary <- subset(plates, plates$pair == "na-pa")

data("san_andreas")
stress_analysis(san_andreas, na_pa, type = "right", plate_boundary, plot = TRUE)
}
}
\seealso{
\code{\link[=PoR_shmax]{PoR_shmax()}}, \code{\link[=distance_from_pb]{distance_from_pb()}}, \code{\link[=norm_chisq]{norm_chisq()}}, \code{\link[=quick_plot]{quick_plot()}}, \code{\link[=circular_summary]{circular_summary()}}
}
