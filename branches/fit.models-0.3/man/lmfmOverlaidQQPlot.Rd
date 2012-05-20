\name{lmfmOverlaidQQPlot}
\alias{lmfmOverlaidQQPlot}

\title{Overlaid Normal QQ Plot of Residuals}

\description{
Produces an overlaid plot comparing residuals QQ plots.
}

\usage{
lmfmOverlaidQQPlot(x, pch, col, ...)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{pch}{a vector with length equal to the number of fitted models in \code{x} specifying the symbols used in the plot. See \code{\link[graphics]{par}} for possible values.}

  \item{col}{a vector with length equal to the number of fitted models in \code{x} specifying the  colors used in the plot. See \code{\link[graphics]{par}} for possible values.}

  \item{\dots}{additional arguments are passed to the low-level plotting functions.}
}

\value{
the \code{trellis} object is invisibly returned.
}

\keyword{hplot}


