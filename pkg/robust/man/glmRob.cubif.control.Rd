\name{glmRob.cubif.control}
\alias{glmRob.cubif.control}

\title{
Control Parameters for the Bounded Influence Robust GLM Estimator
}

\description{
Allows users to set parameters for \code{glmRob}.
}

\usage{
glmRob.cubif.control(epsilon = 0.001, maxit = 50, bpar = 2, cpar = 1.5,
  trc = FALSE, ...)
}

\arguments{
\item{epsilon}{a positive numeric values specifying the convergence threshold for the parameters.}

\item{maxit}{a positive integer giving the maximum number of iterations.}

\item{bpar}{bpar}

\item{cpar}{a single positive numeric value specifying the tuning constant for the initial estimate. This is the truncation value for the likelihood equation for the initial estimate. It determines the starting point of the iterative algorithm to calculate the final estimate.}

\item{trc}{a logical value. If \code{TRUE} the number of the current iteration is printed on the screen.}

\item{\dots}{additional arguments are ignored.}
}

\value{a list is returned containing the values specified in the Arguments section.
}

\seealso{
\code{\link{glmRob}}. 
}

\keyword{robust}
\keyword{regression}
