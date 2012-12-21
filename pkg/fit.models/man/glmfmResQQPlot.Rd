\name{glmfmResQQPlot}
\alias{glmfmResQQPlot}

\title{Residual QQ Plot}

\description{
Produces a side-by-side QQ plot of the residuals versus the appropriate theoretical quantiles.
}

\usage{
glmfmResQQPlot(x, type = "deviance", id.n = 3, qqline = TRUE, \dots)
}

\arguments{
  \item{x}{an glmfm object.}

  \item{type}{a character string specifying the type of residuals used in the plot.  This argument is passed to the generic residuals function when extracting the residuals from \code{x}.}

  \item{id.n}{a non-negative integer value specifying the number of extreme points to identify.}

  \item{qqline}{a logical value.  If TRUE then a qq-line computed using \code{lmRob} is included in the plot.}

  \item{\dots}{any additional arguments are passed to \code{xyplot}.}
}

\value{
the \code{trellis} object is invisibly returned.
}

\keyword{hplot}


