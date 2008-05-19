\name{qqplot.glmRob}
\alias{qqplot.glmRob}

\title{Theoretical Quantiles}

\description{
Compute theoretical quantiles for the deviance residuals obtained from Poisson and binomial regression models.
}

\usage{
qqplot.glmRob(y, par, dist)
}

\arguments{
  \item{y}{a numeric vector containing the response variable.}

  \item{par}{a numeric vector containing the estimated parameter values.}

  \item{dist}{a single integer value specifying the distribution: 0 for binomial; 1 for Poisson.}
}

\value{
A list with the following components:

  \item{quantiles}{a numeric vector containing the theoretical quantiles.}

  \item{deviance.residuals}{a numeric vector containing the deviance residuals.}
}

\keyword{aplot}
