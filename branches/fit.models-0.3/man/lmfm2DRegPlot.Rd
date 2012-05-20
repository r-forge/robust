\name{lmfm2DRegPlot}
\alias{lmfm2DRegPlot}

\title{Scatter Plot with Overlaid Fit(s)}

\description{
Produces a scatter plot of the data with overlaid fits.
}

\usage{
lmfm2DRegPlot(x, lwd, col, ...)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{lwd}{a vector with length equal to the number of fitted models in \code{x} specifying the line widths used in the plot. See \code{\link[graphics]{par}} for possible values.}

  \item{col}{a vector with length equal to the number of fitted models in \code{x} specifying the line colors used in the plot. See \code{\link[graphics]{par}} for possible values.}

  \item{\dots}{additional arguments are passed to \code{\link[lattice]{xyplot}}.}
}

\value{
the \code{trellis} object is invisibly returned.
}

\keyword{hplot}


