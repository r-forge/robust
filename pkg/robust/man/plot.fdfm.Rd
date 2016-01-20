\name{plot.fdfm}
\alias{plot.fdfm}

\title{fdfm Plot Method}

\description{
Comparison plots for fitted univariate distributions.
}

\usage{
\method{plot}{fdfm}(x, which.plots = 2:3, \dots)
}

\arguments{
  \item{x}{an \code{fdfm} object.}

  \item{which.plots}{either "ask", "all", or an integer vector specifying which plots to draw.  In the latter case, use the plot numbers given in the "ask" menu.}

  \item{\dots}{additional arguments are passed to the plotting functions.}
}

\value{
  \code{x} is invisibly returned.
}

\examples{
  data(los, package = "robustbase")
  los.fm <- fit.models(c(Robust = "fitdstnRob", MLE = "fitdstn"),
                         x = los, densfun = "gamma")
  plot(los.fm)
}

\keyword{hplot}


