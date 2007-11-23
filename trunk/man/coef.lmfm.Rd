\name{coef.lmfm}
\alias{coef.lmfm}

\title{Extract Model Coefficients}

\description{
Extract the model coefficients from an lmfm object.
}

\usage{
\method{coef}{lmfm}(object, ...)
}

\arguments{
  \item{object}{an lmfm object.}

  \item{\dots}{additional arguments required by the generic \code{\link{coef}} function.}
}

\value{
a numeric matrix with one row for each model in \code{object} and one column for each coefficient in the model.
}

\examples{
data(stack.dat)
stack.fm <- fit.models(list(Robust = "lmRob", LS = "lm"), Loss ~ ., data = stack.dat)
coef(stack.fm)
}

\keyword{methods}


