\name{OverlaidDenPlot.fdfm}
\alias{OverlaidDenPlot.fdfm}

\title{Overlaid Density Plot}

\description{
Plots the estimated densities over a histogram of the data.
}

\usage{
OverlaidDenPlot.fdfm(x, truncate = 0.999, ...)
}

\arguments{
  \item{x}{an \code{fdfm} object.}

  \item{truncate}{if non NULL, the maximum x-value of the plot is the largest \code{truncate} quantile among the estimated distributions.}

  \item{\dots}{additional arguments are passed to the plotting functions.}
}

\value{
  \code{x} is invisibly returned.
}

\keyword{robust}


