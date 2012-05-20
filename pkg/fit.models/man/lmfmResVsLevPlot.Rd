\name{lmfmResVsLevPlot}
\alias{lmfmResVsLevPlot}

\title{Residuals vs. Leverages Plot}

\description{
Produces a side-by-side plot of the residuals versus leverages.
}

\usage{
lmfmResVsLevPlot(x, type = "response", id.n = 3, ...)
}

\arguments{
  \item{x}{an \code{lmfm} object.}

  \item{type}{a character string specifying the type of residuals used in the plot.  This argument is passed to the generic residuals function when extracting the residuals from \code{x}.}

  \item{id.n}{a non-negative integer value specifying the number of extreme points to identify.}

  \item{\dots}{any additional arguments are passed to \code{xyplot}.}
}

\value{
the \code{trellis} object is invisibly returned.
}

\keyword{hplot}


