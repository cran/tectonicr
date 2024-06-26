% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{tectonicr.colors}
\alias{tectonicr.colors}
\title{Colors for input variables}
\usage{
tectonicr.colors(
  x,
  n = 10,
  pal = NULL,
  categorical = FALSE,
  na.value = "grey",
  ...
)
}
\arguments{
\item{x}{values for color assignment}

\item{n}{integer. number of colors for continuous colors (i.e.
`categorical = FALSE``).}

\item{pal}{either a named vector specifying the colors for categorical
values, or a color function. If \code{NULL}, default colors are
\code{RColorBrewer::brewer.pal()}
(\code{categorical = TRUE}) and \code{viridis::viridis()} (\code{categorical = FALSE}).}

\item{categorical}{logical.}

\item{na.value}{color for \code{NA} values (categorical).}

\item{...}{optional arguments passed to palette function}
}
\value{
named color vector
}
\description{
assigns colors to continuous or categorical values for plotting
}
\examples{
val1 <- c("N", "S", "T", "T", NA)
tectonicr.colors(val1, categorical = TRUE)
tectonicr.colors(val1, pal = stress_colors(), categorical = TRUE)

val2 <- runif(10)
tectonicr.colors(val2, n = 5)
}
