\name{lmfmResQQPlot}
\alias{lmfmResQQPlot}

\title{Residual QQ Plot}

\description{
Produces a side-by-side QQ plot os the residuals against standard normal quantiles.  A simulated confidence envelope is provided for each plot.
}

\usage{
lmfmResQQPlot(x, type = "response", envelope = TRUE, half.normal = FALSE, n.samples = 250, level = 0.95, id.n = 3, robustQQline = TRUE, main, xlab, ylab, ...)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{type}{a character string specifying the type of residuals used in the plot.  This argument is passed to the generic residuals function when extracting the residuals from \code{x}.}

  \item{envelope}{a logical value.  If TRUE a \code{level} confidence envelope is simulated for each qq-plot.}

  \item{half.normal}{a logical value.  If TRUE the plot is drawn using the absolute values of the residuals.}

  \item{n.samples}{a positive integer value giving the number of samples to compute in the simulation of the confidence envelope.}

  \item{level}{a numeric value between 0 and 1 specifying the confidence level for the envelope.}

  \item{id.n}{a non-negative integer value specifying the number of extreme points to identify.}

  \item{robustQQline}{a logical value.  If TRUE then a qq-line computed using \code{\link{lmRob}} is included in the plot.}

  \item{main}{a character string specifying the plot title.}

  \item{xlab}{a character string specifying the x-axis label.}

  \item{ylab}{a character string specifying the y-axis label.}

  \item{\dots}{any additional arguments are passed to \code{xyplot}.}
}

\value{
the \code{trellis} object is invisibly returned.
}

\keyword{hplot}


