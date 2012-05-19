\name{fdfmOverlaidDenPlot}
\alias{fdfmOverlaidDenPlot}

\title{Overlaid Density Plot}

\description{
Plots the estimated densities over a histogram of the data.
}

\usage{
fdfmOverlaidDenPlot(x, truncate = 0.99, ...)
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


