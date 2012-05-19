\name{lmRob.fit}
\alias{lmRob.fit}
\alias{lmRob.wfit}

\title{Robust Fitter Functions for Linear Models}

\description{
These are the basic computing engines called by \code{lmRob} used to robustly fit linear models.  These functions are not intended to be used directly.
}

\usage{
lmRob.fit(x, y, x1.idx = NULL, nrep = NULL, robust.control = NULL, ...)

lmRob.wfit(x, y, w, x1.idx = NULL, nrep = NULL, robust.control = NULL, ...)
}

\arguments{
  \item{x}{a numeric matrix containing the design matrix.}

  \item{y}{a numeric vector containing the linear model response.}

  \item{w}{a numeric vector containing the weights.}

  \item{x1.idx}{a numeric vector containing the indices of columns of the design matrix arising from the coding of factor variables.}

  \item{nrep}{the number of random subsamples to be drawn.  If \code{"Exhaustive"} resampling is being used, the value of \code{nrep} is ignored.}

  \item{robust.control}{a list of control parameters to be used in the numerical algorithms. See \code{lmRob.control} for the possible control parameters and their default settings.}

  \item{\dots}{additional arguments.}
}

\keyword{robust}


