\name{print.summary.glmfm}
\alias{print.summary.glmfm}

\title{Print Method for summary.glmfm Objects}

\description{
Displays a side-by-side comparison of the summaries in a summary.glmfm object.
}

\usage{
\method{print}{summary.glmfm}(x, ...)
}

\arguments{
  \item{x}{a summary.glmfm object.}

  \item{\dots}{additional arguments required by the generic \code{\link{print}} function.}
}

\value{
\code{x} is invisibly returned.
}

\seealso{
\code{\link{summary.glmfm}}.
}


\examples{
data(breslow.dat)
bres.fm <- fit.models(list(Robust = "glmRob", Classical = "glm"),
                      formula = sumY ~ Age10 + Base4*Trt,
                      family = poisson(), data = breslow.dat)
bres.sum <- summary(bres.fm)
print(bres.sum)
print(bres.sum, digits = 4)
}

\keyword{methods}


