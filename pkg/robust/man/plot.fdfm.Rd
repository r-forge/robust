\name{plot.fdfm}
\alias{plot.fdfm}

\title{fdfm Plot Method}

\description{
Comparison plots for estimated univariate distributions.
}

\usage{
\method{plot}{fdfm}(x, which.plots = ifelse(interactive(), "ask", "all"), ...)
}

\arguments{
  \item{x}{an \code{fdfm} object.}

  \item{which.plots}{either "ask", "all", or an integer vector specifying which plots to draw.  If \code{which.plots} is an integer vector, use the plot numbers given in the description above (or in the "ask" menu).}

  \item{\dots}{additional arguments are passed to the plotting functions.}
}

\value{
  \code{x} is invisibly returned.
}

\keyword{robust}


