% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpolation.R
\name{kernel_dispersion}
\alias{kernel_dispersion}
\alias{dispersion_grid}
\title{Adaptive Kernel Dispersion}
\usage{
kernel_dispersion(
  x,
  stat = c("dispersion", "nchisq", "rayleigh"),
  grid = NULL,
  lon_range = NULL,
  lat_range = NULL,
  gridsize = 2.5,
  min_data = 3L,
  max_data = Inf,
  min_dist_threshold = 200,
  dist_threshold = 0.1,
  stat_threshold = Inf,
  R_range = seq(100, 2000, 100),
  ...
)

dispersion_grid(...)
}
\arguments{
\item{x}{\code{sf} object containing
\describe{
\item{azi}{Azimuth in degree}
\item{unc}{Uncertainties of azimuth in degree}
\item{prd}{Predicted value for azimuth}
}}

\item{stat}{The measurement of dispersion to be calculated. Either
\code{"dispersion"} (default), \code{"nchisq"}, or \code{"rayleigh"} for circular
dispersion, normalized Chi-squared test statistic, or Rayleigh test
statistic.}

\item{grid}{(optional) Point object of class \code{sf}.}

\item{lon_range, lat_range}{(optional) numeric vector specifying the minimum
and maximum longitudes and latitudes (are ignored if \code{"grid"} is specified).}

\item{gridsize}{Numeric. Target spacing of the regular grid in decimal
degree. Default is 2.5. (is ignored if \code{"grid"} is specified)}

\item{min_data}{Integer. Minimum number of data per bin. Default is \code{3}}

\item{max_data}{integer. The number of nearest observations that should be
used for prediction, where "nearest" is defined in terms of the space of the
spatial locations. Default is \code{Inf}.}

\item{min_dist_threshold}{Numeric. Maximum distance (in km) of the grid point to the
next data point. Default is 200}

\item{dist_threshold}{Numeric. Distance weight to prevent overweight of data
nearby (\code{0} to \code{1}). Default is \code{0.1}}

\item{stat_threshold}{numeric. Generates missing values when the kernel
\code{stat} value exceeds this threshold. Default is \code{Inf}.}

\item{R_range}{Numeric value or vector specifying the (adaptive) kernel
half-width(s) as search radius (in km). Default is \code{seq(50, 1000, 50)}}

\item{...}{optional arguments to \code{\link[=dist_greatcircle]{dist_greatcircle()}}}
}
\value{
\code{sf} object containing
\describe{
\item{lon,lat}{longitude and latitude in degree}
\item{stat}{output of function defined in \code{stat}}
\item{R}{The rearch radius in km.}
\item{mdr}{Mean distance of datapoints per search radius}
\item{N}{Number of data points in search radius}
}
}
\description{
Stress field and wavelength analysis using circular dispersion
(or other statistical estimators for dispersion)
}
\note{
\code{dispersion_grid()} was renamed to \code{kernel_dispersion()} to create
a more consistent API.
}
\examples{
data("nuvel1")
PoR <- subset(nuvel1, nuvel1$plate.rot == "na")
san_andreas_por <- san_andreas
san_andreas_por$azi <- PoR_shmax(san_andreas, PoR, "right")$azi.PoR
san_andreas_por$prd <- 135
kernel_dispersion(san_andreas_por) |> head()
}
\seealso{
\code{\link[=circular_dispersion]{circular_dispersion()}}, \code{\link[=norm_chisq]{norm_chisq()}}, \code{\link[=rayleigh_test]{rayleigh_test()}}
}
\keyword{internal}
