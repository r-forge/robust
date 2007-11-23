\name{s.logistic.misclass.fit}
\alias{s.logistic.misclass.fit}

\title{S Logistic Misclassification Fitter}

\description{
Computes the estimate for the consistent misclassification method.
}

\usage{
s.logistic.misclass.fit(x, y, mc.gamma, maxit, mc.trc, mc.tol, beta1)
}

\arguments{
  \item{x}{model matrix.}
  \item{y}{response.}
  \item{mc.gamma}{gamma parameter.}
  \item{maxit}{maximum number of iterations.}
  \item{mc.trc}{a logical value. If \code{TRUE} diagnostic information is displayed during the computation of the fit.}
  \item{mc.tol}{tolerance.}
  \item{beta1}{initial parameter estimates.}
}

\value{
a list containing the parameter estimates.
}

\seealso{\code{\link{glmRob.misclass}}}

\keyword{robust}
\keyword{regression}


