\name{summary.lmfm}
\alias{summary.lmfm}

\title{Summary Method for lmfm Objects}

\description{
Compute a summary of each lm or lmRob model in a summary.lmfm object.
}

\usage{
\method{summary}{lmfm}(object, correlation = FALSE, ...)
}

\arguments{
  \item{object}{an lmfm object.}

  \item{correlation}{a logical value.  If TRUE, the correlation matrices for the coefficients are included in the summaries.}

  \item{...}{additional arguments required by the generic \code{\link{summary}} function.}
}

\value{
a list with class summary.lmfm whose elements are summaries of each model in \code{object}.  
}

\seealso{
\code{\link{fit.models}},
\code{\link{summary}},
\code{\link{summary.lmRob}},
\code{\link{summary.lm}}.
}

\examples{
data(stack.dat)
stack.fm <- fit.models(list(Robust = "lmRob", LS = "lm"), Loss ~ ., data = stack.dat)
stack.sum <- summary(stack.fm)
stack.sum
}

\keyword{methods}
\keyword{robust}
\keyword{regression}


