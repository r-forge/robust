\name{indexPlot.lmfm}
\alias{indexPlot.lmfm}

\title{Comparison Index (Time) Plot}

\description{
Produces side-by-side index (time) plots.
}

\usage{
indexPlot.lmfm(x, fun, level = 0.95, id.n = 3, \dots)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{fun}{a function to extract the appropriate quantity from \code{x}.}

  \item{level}{a numeric value between 0 and 1 specifying the confidence level used to draw the threshold in the plot.}

  \item{id.n}{a non-negative integer value specifying the number of extreme points to identify.}

  \item{\dots}{any additional arguments are passed to \code{\link[lattice]{xyplot}}.}
}

\value{
the \code{trellis} object is invisibly returned.
}

\keyword{hplot}


