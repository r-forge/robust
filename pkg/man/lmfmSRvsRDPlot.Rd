\name{lmfmSRvsRDPlot}
\alias{lmfmSRvsRDPlot}

\title{Standardized Residuals versus Robust Distances Plot}

\description{
Produces side-by-side residuals versus robust distances plots from an lmfm object.
}

\usage{
lmfmSRvsRDPlot(x, type = "response", level = 0.95, id.n = 3, main, xlab, ylab, ...)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{type}{a character string specifying the type of residuals used in the plot.  This argument is passed to the generic residuals function when extracting the residuals from \code{x}.}

  \item{level}{a numeric value between 0 and 1 giving the confidence level used to draw the threshold for both the distances and residuals axes.}

  \item{id.n}{a non-negative integer value specifying the number of extreme points to identify.}

  \item{main}{a character string specifying the plot title.}

  \item{xlab}{a character string specifying the x-axis label.}

  \item{ylab}{a character string specifying the y-axis label.}

  \item{\dots}{additional arguments are passed to the low-level plotting functions.}
}

\value{
\code{x} is invisibly returned.
}

\keyword{hplot}


