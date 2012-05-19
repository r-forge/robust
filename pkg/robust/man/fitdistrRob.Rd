\name{fitdistrRob}
\alias{fitdistrRob}

\title{Robust Fitting of Univariate Distributions}

\description{
Robust Fitting of Univariate Distributions.
}

\usage{
fitdistrRob(x, densfun, ...)
}

\arguments{
  \item{x}{A numeric vector containing the sample.}

  \item{densfun}{a character string naming the distribution. Distributions \sQuote{gamma}, \sQuote{lognormal}, and \sQuote{weibull} are recognized.}

  \item{\dots}{additional arguments are passed to the underlying functions.}
}

\value{
a list with class "fitdistrRob" inheriting from "fitdistr".

  \item{estimate}{a named numeric vector containing the parameter estimates.}

  \item{sd}{a named numeric vector containing the standard deviations of the parameter estimates.}

  \item{vcov}{a numeric matrix containing the variance-covariance matrix of the estimated parameter vector.}

  \item{loglik}{currently missing.}

  \item{mu}{a single numeric value containing an estimate of the mean.}

  \item{V.mu}{a single numeric value containing the variance of the estimated mean.}

  \item{call}{the matched call.}

  \item{control}{a list containing the control parameters used by the estimator.}
}

\seealso{
\code{\link{gammaRob}}, \code{\link{lognormRob}}, \code{\link{weibullRob}}, and \code{\link[MASS]{fitdistr}}.
}

\keyword{robust}


