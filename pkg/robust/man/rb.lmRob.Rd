\name{rb.lmRob}
\alias{rb.lmRob}

\title{Robust Bootstrap Standard Errors}

\description{
Computes a robust bootstrap estimate of the standard error for each coefficient estimate in a robustly fitted linear model.  This function is called by \code{\link{summary.lmRob}} and is not intended to be called directly by users.
}

\usage{
rb.lmRob(lmRob.object, M = 1000, seed = 99, fixed = TRUE)
}

\arguments{
\item{lmRob.object}{an lmRob object.}

\item{M}{a positive integer giving the number of bootstrap subsamples.}

\item{seed}{a positive integer specifying the seed for the random number generator.}

\item{fixed}{a logical value.  This should be set to \code{TRUE}.}
}

\value{
a numeric vector of robust bootstrap standard error estimates.
}

\seealso{
\code{\link{lmRob}}, 
\code{\link{summary.lmRob}}.
}

\keyword{robust}
\keyword{regression}


