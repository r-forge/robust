\name{overlaidQQPlot.lmfm}
\alias{overlaidQQPlot.lmfm}

\title{Overlaid Normal QQ Plot}

\description{
Produces an overlaid normal QQ plot.
}

\usage{
overlaidQQPlot.lmfm(x, fun, \dots)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{fun}{a function to extract the appropriate quantity from \code{x}.}

  \item{\dots}{additional arguments are passed to \code{\link[lattice]{qqmath}}.}
}

\value{
the \code{trellis} object is invisibly returned.
}

\keyword{hplot}


