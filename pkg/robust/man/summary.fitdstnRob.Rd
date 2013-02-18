\name{summary.fitdstn}
\alias{summary.fitdstn}

\title{fitdstn Summary Method}

\description{Summary for univariate parameter estimates.}

\usage{
\method{summary}{fitdstn}(object, \dots)
}

\arguments{
  \item{object}{a \code{fitdstn} object.}

  \item{\dots}{additional arguments are ignored.}
}

\value{
  a list with class \code{summary.fitdstn} containing the following elements:

  \item{call}{the matched call.}

  \item{estimate}{a named numeric vector containing the parameter estimates.}

  \item{sd}{a named numeric vector containing the standard deviations of the parameter estimates.}

  \item{vcov}{a numeric matrix containing the variance-covariance matrix of the estimated parameter vector.}

  \item{mu}{a single numeric value containing an estimate of the mean.}

  \item{V.mu}{a single numeric value containing the variance of the estimated mean.}
}

\keyword{methods}


