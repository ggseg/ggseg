% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom-brain.R
\docType{data}
\name{geom_brain}
\alias{geom_brain}
\alias{GeomBrain}
\title{Brain geom}
\usage{
geom_brain(
  mapping = aes(),
  data = NULL,
  atlas,
  hemi = NULL,
  side = NULL,
  position = position_brain(),
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)
}
\arguments{
\item{mapping}{argument to pass to \code{\link[ggplot2]{aes}} to map
variables from the supplied data to the plot}

\item{data}{data.frame with data to plot}

\item{atlas}{object of type brain_atlas to plot}

\item{hemi}{hemisphere to plot. Defaults to everything in the atlas.}

\item{side}{slice to plot, as recorded in the "side" column in the atlas data. Defaults to all.}

\item{position}{position of the data. Default is "identity" but can be
changed by \code{\link{position_brain}}.}

\item{show.legend}{logical. Should legend be added or not.}

\item{inherit.aes}{logical. if aes should be inherited from the
main ggplot call or not}

\item{...}{arguments to \code{\link[ggplot2]{geom_sf}}}
}
\value{
ggplot object
}
\description{
call to \code{\link[ggplot2]{geom_sf}}
}
\examples{
library(ggplot2)

ggplot() +
 geom_brain(atlas = dk)
}
\keyword{datasets}
