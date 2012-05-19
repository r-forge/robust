\name{gammaRob}
\alias{gammaRob}

\title{Robust Estimation of Gamma Distribution Parameters}

\description{
Robust estimation of gamma distribution parameters.
}

\usage{
gammaRob(x, estim = c("M", "tdmean"), control = gammaRob.control(estim, ...), ...)
}


\arguments{
  \item{x}{A numeric vector containing the sample.}

  \item{estim}{a character string specifying the estimator to use.}

  \item{control}{a list of control parameters appropriate for the estimator in \code{estim}.}

  \item{\dots}{control parameters may also be given here.}
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
\code{\link{gammaRob.control}}, \code{\link{fitdistrRob}}, and \code{\link[MASS]{fitdistr}}.
}

\keyword{robust}


