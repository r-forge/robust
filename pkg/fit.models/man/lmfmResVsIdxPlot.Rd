\name{lmfmResVsIdxPlot}
\alias{lmfmResVsIdxPlot}

\title{Residuals vs. Index (Time) Plot}

\description{
Produces a side-by-side plot of the residuals vs. index (time).
}

\usage{
lmfmResVsIdxPlot(x, residuals.fun, level = 0.95, id.n = 3, \dots)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{residuals.fun}{a function to extract the residuals from \code{x}.}

  \item{level}{a numeric value between 0 and 1 specifying the confidence level used to draw the threshold in the plot.}

  \item{id.n}{a non-negative integer value specifying the number of extreme points to identify.}

  \item{\dots}{any additional arguments are passed to \code{xyplot}.}
}

\value{
the \code{trellis} object is invisibly returned.
}

\keyword{hplot}


