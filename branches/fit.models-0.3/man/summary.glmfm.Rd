\name{summary.glmfm}
\alias{summary.glmfm}

\title{Summary Method for glmfm Objects}

\description{
Compute a summary of each glm or glmRob model in a glmfm object.
}

\usage{
\method{summary}{glmfm}(object, correlation = FALSE, ...)
}

\arguments{
\item{object}{a glmfm object.}

\item{correlation}{a logical value. If \code{TRUE}, correlation matrices of the coefficient estimates are included in each summary.}

\item{\dots}{additional arguments required by the generic \code{\link{summary}} function.}
}

\value{
a list with class summary.glmfm whose elements are summaries of each model in \code{object}.  
}

\seealso{
\code{\link{fit.models}},
\code{\link{summary}},
\code{\link{summary.glm}}.
}

\examples{
#data(breslow.dat)
#bres.fm <- fit.models(list(Robust = "glmRob", Classical = "glm"),
#                      formula = sumY ~ Age10 + Base4*Trt,
#                      family = poisson(), data = breslow.dat)
#bres.sum <- summary(bres.fm)
#bres.sum
}

\keyword{methods}
\keyword{regression}

