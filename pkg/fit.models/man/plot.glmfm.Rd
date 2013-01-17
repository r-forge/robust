\name{plot.glmfm}
\alias{plot.glmfm}

\title{Comparison Plots for Generalized Linear Models}

\description{
Produces a set of comparison diagnostic plots.  The plot options are
\describe{
  \item{(2)}{Deviance Residuals vs. Fitted Values,}
  \item{(3)}{Response vs. Fitted Values,}
  \item{(4)}{Normal QQ Plot of Pearson Residuals,}
  \item{(5)}{QQ Plot of Deviance Residuals,}
  \item{(6)}{Deviance Residuals vs. Leverage,}
  \item{(7)}{Deviance Residuals vs. Index (Time),}
  \item{(8)}{Scale-Location.}
}
}

\usage{
\method{plot}{glmfm}(x, which.plots = c(2, 5, 7, 6), \dots)
}

\arguments{
  \item{x}{a \code{"glmfm"} object.}

\item{which.plots}{either \code{"ask"}, \code{"all"}, or a vector of integer values specifying which plots to draw.  In the latter case, use the plot numbers given in the description above (or in the "ask" menu).  Any other values will be silently ignored.}

\item{\dots}{other parameters to be passed through to plotting functions.}
}

\section{Side Effects}{
The selected plots are drawn on a graphics device.
}

\value{
\code{x} is invisibly returned.
}

\seealso{
See \code{\link{glmfmResQQPlot}} for (2), \code{\link{qqPlot.lmfm}} for (4), and \code{\link{scatterPlot.lmfm}} for the others.
}

\keyword{hplot}
\keyword{methods}


