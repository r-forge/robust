\name{lmfmOverlaidResDenPlot}
\alias{lmfmOverlaidResDenPlot}

\title{Overlaid Kernel Density Estimate of Residuals}

\description{
Produces an overlaid plot comparing kernel density estimates of the residuals.
}

\usage{
lmfmOverlaidResDenPlot(x, lty, lwd, col, ...)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{lty}{a vector with length equal to the number of fitted models in \code{x} specifying the line styles used in the plot. See \code{\link[graphics]{par}} for possible values.}

  \item{lwd}{a vector with length equal to the number of fitted models in \code{x} specifying the line widths used in the plot. See \code{\link[graphics]{par}} for possible values.}

  \item{col}{a vector with length equal to the number of fitted models in \code{x} specifying the line colors used in the plot. See \code{\link[graphics]{par}} for possible values.}

  \item{\dots}{additional arguments are passed to the low-level plotting functions.}
}

\value{
the \code{trellis} object is invisibly returned.
}

\keyword{hplot}


