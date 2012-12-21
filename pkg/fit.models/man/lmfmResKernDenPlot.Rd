\name{lmfmResKernDenPlot}
\alias{lmfmResKernDenPlot}

\title{Kernel Density Estimate of Residuals}

\description{
Produces a side-by-side plot of kernel density estimates of the residuals.
}

\usage{
lmfmResKernDenPlot(x, residuals.fun, ...)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{residuals.fun}{a function to extract the residuals from \code{x}.}

  \item{\dots}{any additional arguments are passed to \code{xyplot}.}
}

\value{
the \code{trellis} object is invisibly returned.
}

\keyword{hplot}


