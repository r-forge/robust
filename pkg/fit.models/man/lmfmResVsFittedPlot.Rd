\name{lmfmResVsFittedPlot}
\alias{lmfmResVsFittedPlot}

\title{Residuals versus Fitted Values Plot}

\description{
Produces a side-by-side plot of the residuals versus the fitted values.
}

\usage{
lmfmResVsFittedPlot(x, residuals.fun, smooths = FALSE, rugplot = FALSE,
                    id.n = 3, \dots)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{residuals.fun}{a function to extract the residuals from \code{x}.}

  \item{smooths}{a logical value.  If TRUE then a smooth curve computed using \code{\link{loess}} is included in each panel of the plot.}

  \item{rugplot}{a logical value.  If TRUE then a rugplot is included in each panel of the plot.}

  \item{id.n}{a non-negative integer value giving the number of extreme points to identify.}

  \item{\dots}{any additional arguments are passed to \code{xyplot}.}
}

\value{
the \code{trellis} object is invisibly returned.
}

\keyword{hplot}


