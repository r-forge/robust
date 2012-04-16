\name{lmfmSqrtResVsFittedPlot}
\alias{lmfmSqrtResVsFittedPlot}

\title{Square Root of Absolute Residuals vs. Fitted Values Plot}

\description{
Produces a side-by-side plot of the square root of the absolute values of the residuals versus the fitted values.
}

\usage{
lmfmSqrtResVsFittedPlot(x, type = "response", smooths = FALSE, rugplot = FALSE, id.n = 3, main, xlab, ylab, ...)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{type}{a character string specifying the type of residuals used in the plot.  This argument is passed to the generic residuals function when extracting the residuals from \code{x}.}

  \item{smooths}{a logical value.  If TRUE then a smooth curve computed using \code{\link{loess}} is included in each panel of the plot.}

  \item{rugplot}{a logical value.  If TRUE then a rugplot is included in each panel of the plot.}

  \item{id.n}{a non-negative integer value giving the number of extreme points to identify.}

  \item{main}{a character string specifying the plot title.}

  \item{xlab}{a character string specifying the x-axis label.}

  \item{ylab}{a character string specifying the y-axis label.}

  \item{\dots}{any additional arguments are passed to \code{xyplot}.}
}

\value{
the \code{trellis} object is invisibly returned.
}

\keyword{hplot}


