\name{print.lmfm}
\alias{print.lmfm}

\title{Print Method for lmfm Objects}

\description{
Prints a brief description of the models contained in an lmfm object.
}

\usage{
\method{print}{lmfm}(x, ...)
}

\arguments{
  \item{x}{an lmfm object.}

  \item{\dots}{additional arguments required by the generic \code{\link{print}} function.}
}

\value{
\code{x} is invisibly returned.
}

\examples{
data(stack.dat)
stack.fm <- fit.models(list(Robust = "lmRob", LS = "lm"), Loss ~ ., data = stack.dat)
print(stack.fm)
}

\keyword{methods}


