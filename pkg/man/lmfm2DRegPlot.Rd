\name{lmfm2DRegPlot}
\alias{lmfm2DRegPlot}

\title{Data with Fit}

\description{
Plots the linear model fits in an lmfm object over a scatterplot of the data.
}

\usage{
lmfm2DRegPlot(x, cutoff = TRUE, main, xlab, ylab, ...)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{cutoff}{a logical value.  If TRUE then the threshold beyond which points receive zero weight is plotted with dashed lines.}

  \item{main}{a character string specifying the plot title.}

  \item{xlab}{a character string specifying the x-axis label.}

  \item{ylab}{a character string specifying the y-axis label.}

  \item{\dots}{additional arguments are passed to the low-level plotting functions.}
}

\value{
\code{x} is invisibly returned.
}

\keyword{hplot}


