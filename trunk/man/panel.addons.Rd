\name{panel.addons}
\alias{panel.addons}

\title{Add Extras to fit.models Plots}

\description{
This is a utility function used by some of the plotting functions in the Robust Library.  It optionally adds a smooth fit using loess, a rugplot, and/or identifies a specified number of extreme observations.
}

\usage{
panel.addons(x, y, smooths = FALSE, rugplot = FALSE, id.n = 3)
}

\arguments{
  \item{x}{a numeric vector of x-coordinates.}

  \item{y}{a numeric vector of y-coordinates.  Must be the same length as \code{x}.}

  \item{smooths}{a logical value.  If \code{TRUE} then a smooth fit computed by loess is included in each panel.}

  \item{rugplot}{a logical value.  If \code{TRUE} then a rugplot is included in each panel.}

  \item{id.n}{a non-negative integer value specifying the number of extreme points to label in each panel.}

  \item{\dots}{additional arguments are passed to the low-level plotting functions.}
}

\value{
a \code{NULL} value is invisibly returned.
}

\keyword{aplot}


