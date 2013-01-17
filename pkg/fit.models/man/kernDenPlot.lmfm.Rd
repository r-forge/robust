\name{kernDenPlot.lmfm}
\alias{kernDenPlot.lmfm}

\title{Comparison Kernel Density Estimate Plot}

\description{
Produces side-by-side kernel density estimate plots.
}

\usage{
kernDenPlot.lmfm(x, fun, ...)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{fun}{a function to extract the appropriate quantity from \code{x}.}

  \item{\dots}{additional arguments are passed to \code{\link[lattice]{xyplot}}.}
}

\value{
the \code{trellis} object is invisibly returned.
}

\keyword{hplot}


