\name{summary.glmfm}
\alias{summary.glmfm}

\title{Summary Method for glmfm Objects}

\description{
Compute a summary of each glm or glmRob model in a glmfm object.
}

\usage{
\method{summary}{glmfm}(object, correlation = FALSE, \dots)
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
# From ?glm:
# A Gamma example, from McCullagh & Nelder (1989, pp. 300-2)

clotting <- data.frame(
    u = c(5,10,15,20,30,40,60,80,100),
    lot1 = c(118,58,42,35,27,25,21,19,18),
    lot2 = c(69,35,26,21,18,16,13,12,12))


lot1 <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
lot2 <- glm(lot2 ~ log(u), data = clotting, family = Gamma)

fm <- fit.models(lot1, lot2)
summary(fm)
}

\keyword{regression}
\keyword{methods}

