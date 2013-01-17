\name{simpleRegPlot.lmfm}
\alias{simpleRegPlot.lmfm}

\title{Scatter Plot with Overlaid Fits}

\description{
Produces a scatter plot of the data with overlaid fits.
}

\usage{
simpleRegPlot.lmfm(x, lwd.reg, col.reg, \dots)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{lwd.reg}{a vector with length equal to the number of fitted models in \code{x} specifying the line widths used in the plot. See \code{\link[graphics]{par}} for possible values.}

  \item{col.reg}{a vector with length equal to the number of fitted models in \code{x} specifying the line colors used in the plot. See \code{\link[graphics]{par}} for possible values.}

  \item{\dots}{additional arguments are passed to \code{\link[lattice]{xyplot}}.}
}

\value{
the \code{trellis} object is invisibly returned.
}

\keyword{hplot}


