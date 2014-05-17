\name{lsRobTest}
\alias{lsRobTest}

\title{
Bias Test for Least-Squares Regression Estimates
}

\description{
Test for bias between least-squares and robust MM linear regression estimates.
}

\usage{
lsRobTest(object, test = c("T2", "T1"), ...)
}

\arguments{
  \item{object}{an \code{lmRob} object (the output of the \code{\link{lmRob}} function).}

  \item{test}{either \code{"T1"} or \code{"T2"}. Selects the null hypothesis. T2 (the default ): the error distribution not bias inducing. T1: the residual error distribution is normal.}

  \item{\dots}{additional arguments are ignored.}
}

\examples{
rob.fit <- lmRob(stack.loss ~ ., data = stackloss)
lsRobTest(rob.fit)
lsRobTest(rob.fit, test = "T1")
}

\keyword{robust}
\keyword{regression}


