\name{gen.data}
\alias{gen.data}

\title{Generate Data With Contamination}

\description{
Generates a random dataset with some amount of contaimination.
}

\usage{
gen.data(coeff, n = 100, eps = 0.1, sig = 3, snr = 1/20, seed = 837)
}

\arguments{
  \item{coeff}{a numeric vector of length 3 containing the true coefficients.}

  \item{n}{a positive integer giving the number of observations in the data set.}

  \item{eps}{a numeric value between 0 and 0.5 specifying the fraction of contamination.}

  \item{sig}{a positive numeric value giving the standard deviation of the uncontaminated data.}

  \item{snr}{a positive numeic value giving the signal to noise ratio, well not really.}

  \item{seed}{an integer value giving the seed for the random number generator.}
}

\value{
a data frame with \code{n} rows and 4 columns.  The regressors are generated as: \code{rnorm(n,1)}, \code{rnorm(n,1)^3}, \code{exp(rnorm(n,1))}.  It also generates an unused vector \code{x4}.
}

\keyword{utilities}


