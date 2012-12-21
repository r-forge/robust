\name{lmfmResVsDistPlot}
\alias{lmfmResVsDistPlot}

\title{Residuals versus Design Distance Plot}

\description{
Produces a side-by-side plot of the residuals versus design distance.
}

\usage{
lmfmResVsDistPlot(x, residuals.fun, id.n = 3, \dots)
}

\arguments{
  \item{x}{an \code{lmfm} object.}

  \item{residuals.fun}{a function to extract the residuals from \code{x}.}

  \item{id.n}{a non-negative integer value specifying the number of extreme points to identify.}

  \item{\dots}{any additional arguments are passed to \code{xyplot}.}
}

\value{
the \code{trellis} object is invisibly returned.
}

\keyword{hplot}


