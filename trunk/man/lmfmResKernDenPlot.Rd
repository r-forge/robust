\name{lmfmResKernDenPlot}
\alias{lmfmResKernDenPlot}

\title{Kernel Density Estimate of Residuals}

\description{
Produces a side-by-side plot of kernel density estimates of the residuals.
}

\usage{
lmfmResKernDenPlot(x, n = 512, main, xlab, ylab, ...)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{n}{a positive integer value specifying the number of equally spaced points at which the density is to be estimated.  See \code{\link[stats]{density}}.}

  \item{main}{a character string specifying the plot title.}

  \item{xlab}{a character string specifying the x-axis label.}

  \item{ylab}{a character string specifying the y-axis label.}

  \item{\dots}{any additional arguments are passed to \code{xyplot}.}
}

\value{
the \code{trellis} object is invisibly returned.
}

\keyword{hplot}


