\name{summary.fitdstnRob}
\alias{summary.fitdstnRob}

\title{fitdstnRob Summary Method}

\description{Summary for univariate parameter estimates.}

\usage{
\method{summary}{fitdstnRob}(object, ...)
}

\arguments{
  \item{object}{an \code{fitdstnRob} object.}

  \item{\dots}{additional arguments are ignored.}
}

\value{
  a list with class "summary.fitdstnRob" containing the following elements:

  \item{call}{the matched call.}

  \item{estimate}{a named numeric vector containing the parameter estimates.}

  \item{sd}{a named numeric vector containing the standard deviations of the parameter estimates.}

  \item{vcov}{a numeric matrix containing the variance-covariance matrix of the estimated parameter vector.}

  \item{mu}{a single numeric value containing an estimate of the mean.}

  \item{V.mu}{a single numeric value containing the variance of the estimated mean.}
}

\keyword{robust}


