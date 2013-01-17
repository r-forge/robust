\name{qqPlot.lmfm}
\alias{qqPlot.lmfm}

\title{Compaison QQ Plot}

\description{
Produces side-by-side QQ plots.  Aa optional simulated confidence envelope can be included in each plot.
}

\usage{
qqPlot.lmfm(x, fun, envelope = TRUE, half.normal = FALSE, n.samples = 250,
            level = .95, id.n = 3, qqline = TRUE, \dots)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{fun}{a function to extract the appropriate quantity from \code{x}.}

  \item{envelope}{a logical value.  If \code{TRUE} a \code{level} confidence envelope is simulated for each QQ plot.}

  \item{half.normal}{a logical value.  If \code{TRUE} the plot is drawn using the absolute values.}

  \item{n.samples}{a positive integer value giving the number of samples to compute in the simulation of the confidence envelope.}

  \item{level}{a numeric value between 0 and 1 specifying the confidence level for the envelope.}

  \item{id.n}{a non-negative integer value specifying the number of extreme points to identify.}

  \item{qqline}{a logical value.  If \code{TRUE}, a QQ line is included in the plot.}

  \item{\dots}{additional arguments are passed to \code{\link[lattice]{xyplot}}.}
}

\value{
the \code{trellis} object is invisibly returned.
}

\keyword{hplot}


