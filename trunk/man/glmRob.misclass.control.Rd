\name{glmRob.misclass.control}
\alias{glmRob.misclass.control}

\title{
Control for Misclassification Robust GLM Estimator
}

\description{
Allows users to set parameters for \code{glmRob}.
}

\usage{
glmRob.misclass.control(mc.gamma = 0.01, mc.maxit = 30, mc.trc = FALSE,
  mc.tol = 0.001, mc.initial = NULL, ...)
}

\arguments{

\item{mc.gamma}{a real number between 0 and 1 that represents the probability of
misclassification of a response variable.}

\item{mc.maxit}{maximum number of iterations.}

\item{mc.trc}{a logical value indicating whether a trace of the current parameter values is printed to the screen while the algorithm iterates.}

\item{mc.tol}{convergence threshold.}

\item{mc.initial}{a vector of initial values to start the iterations. If ommited, the coeficients resulting from a non-robust glm fit are used.}

\item{\dots}{additional arguments are ignored.}
}

\value{
a list containing the parameters packaged to be used by \code{\link{glmRob}}. The values for \code{\link{glmRob.misclass.control}} can be supplied directly in a call to \code{\link{glmRob}}. These values are filtered through \code{\link{glmRob.misclass.control}} inside \code{\link{glmRob}}.
}

\seealso{\code{\link{glmRob}}}

\keyword{robust}
\keyword{regression}


