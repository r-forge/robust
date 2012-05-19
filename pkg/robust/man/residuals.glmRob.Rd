\name{residuals.glmRob}
\alias{residuals.glmRob}

\title{Residuals Methods for glmRob Objects}

\description{
  Residuals methods for \code{glmRob} objects.

}
\usage{
\method{residuals}{glmRob}(object, type = c("deviance", "pearson", "working", "response"), ...)
}


\arguments{
  \item{object}{a \code{glmRob} object.}

  \item{type}{the type of residuals to be returned.}

  \item{\dots}{additional arguments are ignored.}
}

\value{
  a numeric vector containing the residuals.
}

\keyword{robust}


