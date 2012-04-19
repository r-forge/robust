\name{lmfmResVsRDPlot}
\alias{lmfmResVsRDPlot}

\title{Residuals vs. Robust Distances Plot}

\description{
Produces a side-by-side plot of the residuals versus the robust distances.
}

\usage{
lmfmResVsRDPlot(x, type = "response", level = 0.95, id.n = 3, ...)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{type}{a character string specifying the type of residuals used in the plot.  This argument is passed to the generic residuals function when extracting the residuals from \code{x}.}

  \item{level}{a numeric value between 0 and 1 giving the confidence level used to draw the threshold for both the distances and residuals axes.}

  \item{id.n}{a non-negative integer value specifying the number of extreme points to identify.}

  \item{\dots}{any additional arguments are passed to \code{xyplot}.}
}

\value{
the \code{trellis} object is invisibly returned.
}

\keyword{hplot}


