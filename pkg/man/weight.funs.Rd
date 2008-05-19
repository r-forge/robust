\name{weight.funs.q}
\alias{psi.weight}
\alias{rho.weight}
\alias{psp.weight}
\alias{chi.weight}

\title{Weight Functions}

\description{
These functions compute the weights used by lmRob and its associated methods.
}

\usage{
psi.weight(svals, ips = 1, xk = 1.06)
rho.weight(svals, ips = 1, xk = 1.06)
psp.weight(svals, ips = 1, xk = 1.06)
chi.weight(svals, ips = 1, xk = 1.06)
}

\arguments{
  \item{svals}{a numeric vector.}

  \item{ips}{choose your weight function: \code{ips = 1} for optimal function, \code{ips = 2} for rescaled bisquare function, and \code{ips = 3} for Huber function.}

  \item{xk}{a numeric value specifying the tuning constant.}
}

\details{
See the Theoretical Details section in chapter 2 of Robust.pdf.
}

\value{
a numeric vector containing the weights.
}

\keyword{robust}


