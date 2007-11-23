\name{lmfmRespVsFittedPlot}
\alias{lmfmRespVsFittedPlot}

\title{Response versus Fitted Values Plot}

\description{
Produces side-by-side response versus fitted value plots from an lmfm object.
}

\usage{
lmfmRespVsFittedPlot(x, smooths = FALSE, rugplot = FALSE, ...)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{smooths}{a logical value.  If TRUE then a smooth curve computed by loess is added to each panel of the plot.}

  \item{rugplot}{a logical value.  If TRUE then a rugplot is added to each panel of the plot.}

  \item{\dots}{additional arguments are passed to the low-level plotting functions.}
}

\value{
\code{x} is invisibly returned.
}

\keyword{hplot}


