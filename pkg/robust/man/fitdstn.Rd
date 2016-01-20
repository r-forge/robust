\name{fitdstn}
\alias{fitdstn}
\title{Maximum-likelihood Fitting of Univariate Distributions}

\description{
Maximum-likelihood fitting of univariate distributions.
}

\usage{
fitdstn(x, densfun, \dots)
}

\arguments{
  \item{x}{a numeric vector containing the sample.}
  \item{densfun}{a character string naming the
  distribution.  Distributions \sQuote{gamma}, \sQuote{lognormal}, and
  \sQuote{weibull} are supported.}
  \item{\dots}{additional arguments are ignored.}
}

\details{
  This function relies on the \code{\link[MASS]{fitdistr}} function for
  the computations.  The returned object is modified to support plotting
  and comparison.
}

\value{
  a list with class \dQuote{fitdstn} containing the following elements:

  \item{estimate}{a named numeric vector containing the parameter estimates.}
  \item{sd}{a named numeric vector containing the standard deviations of
    the parameter estimates.}
    \item{vcov}{a numeric matrix containing the variance-covariance matrix
    of the estimated parameter vector.}
  \item{n}{a single numeric value indicating the number of sample points in \code{x}.}
  \item{loglik}{a single numeric value giving the maxized the log-likelihood.}
  \item{call}{the matched call.}
  \item{densfun}{the character string \code{densfun} provided in the arguments.}
  \item{x}{the data provided in \code{x}.}
}
\note{
  The \code{print} method displays the estimated parameters and their
  standard errors (in parentheses).
}

\seealso{
  An important goal here is the comparison with \emph{robust} fits to
  the same distributions, see \code{\link{fitdstnRob}}.

  \code{\link[MASS]{fitdistr}} which provides many more choices for
  \code{densfun}.
}
\keyword{maximum-likelihood}



