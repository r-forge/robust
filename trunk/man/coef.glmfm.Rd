\name{coef.glmfm}
\alias{coef.glmfm}

\title{Extract Model Coefficients}

\description{
Extract the model coefficients from a glmfm object.
}

\usage{
\method{coef}{glmfm}(object, ...)
}

\arguments{
  \item{object}{a glmfm object.}

  \item{\dots}{additional arguments required by the generic \code{\link{coef}} function.}
}

\value{
a numeric matrix with one row for each model in \code{object} and one column for each coefficient in the model.
}

\examples{
data(breslow.dat)
bres.fm <- fit.models(list(Robust = "glmRob", Classical = "glm"),
                      formula = sumY ~ Age10 + Base4*Trt,
                      family = poisson(), data = breslow.dat)
coef(bres.fm)
}

\keyword{methods}


