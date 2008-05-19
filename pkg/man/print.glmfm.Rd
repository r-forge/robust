\name{print.glmfm}
\alias{print.glmfm}

\title{Print Method for glmfm Objects}

\description{
Displays the calls, coefficient estimates and residual deviance estimates for the glm and/or glmRob objects contained in a glmfm object.
}

\usage{
\method{print}{glmfm}(x, ...)
}

\arguments{
  \item{x}{a glmfm object.}

  \item{\dots}{additional arguments required by the generic print function.  In particular the 'digits' argument is useful for specifying the number of significant digits when displaying numeric output.}
}

\value{
  \code{x} is invisibly returned.
}

\seealso{
\code{\link{fit.models}},
\code{\link{glmRob}},
\code{\link{glm}},
\code{\link{print}}.
}

\examples{
data(breslow.dat)
bres.fm <- fit.models(list(Robust = "glmRob", Classical = "glm"),
                      formula = sumY ~ Age10 + Base4*Trt,
                      family = poisson(), data = breslow.dat)
print(bres.fm)
print(bres.fm, digits = 4)
}

\keyword{methods}


