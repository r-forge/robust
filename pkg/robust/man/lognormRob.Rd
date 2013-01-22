\name{lognormRob}
\alias{lognormRob}

\title{Robust Estimation of Lognormal Distribution Parameters}

\description{
  Robust estimation of lognormal distribution parameters.
}

\usage{
lognormRob(x, estim = c("tdmean"), control = lognormRob.control(estim, ...), ...)
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
\code{\link{lognormRob.control}}, \code{\link{fitdstnRob}}.
}

\keyword{robust}


