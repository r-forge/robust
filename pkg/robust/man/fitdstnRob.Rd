\name{fitdstnRob}
\alias{fitdstnRob}
\title{Robust Fitting of Univariate Distributions}

\description{
Robust Fitting of Univariate Distributions.
}

\usage{
fitdstnRob(x, densfun, \dots)
}
\arguments{
  \item{x}{A numeric vector containing the sample.}
  \item{densfun}{a character string naming the
    distribution.  Distributions \sQuote{gamma}, \sQuote{lognormal}, and
  \sQuote{weibull} are recognized.}
  \item{\dots}{additional arguments are passed to the fitting functions.}
}

\value{
a list with class \dQuote{fitdstn} containing the following elements:

  \item{estimate}{a named numeric vector containing the parameter estimates.}
  \item{sd}{a named numeric vector containing the standard deviations of the parameter estimates.}
  \item{vcov}{a numeric matrix containing the variance-covariance matrix of the estimated parameter vector.}
  \item{mu}{a single numeric value containing an estimate of the mean.}
  \item{V.mu}{a single numeric value containing the variance of the estimated mean.}
  \item{control}{a list containing the control parameters used by the estimator.}
  \item{call}{the matched call.}
  \item{densfun}{the character string \code{densfun} provided in the arguments.}
  \item{x}{the data provided in \code{x}.}

The \code{print} method displays the estimated parameters and their standard errors (in parentheses).
}

\seealso{
  \code{\link{gammaRob}}, \code{\link{lognormRob}},
  \code{\link{weibullRob}}.

  The classical counterparts, see \code{\link{fitdstn}}.
}

\keyword{robust}


