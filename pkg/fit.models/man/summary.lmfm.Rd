\name{summary.lmfm}
\alias{summary.lmfm}

\title{Comparison Summaries for Linear Regression Models}

\description{
Compute a summary of each model in an \code{lmfm} object.
}

\usage{
\method{summary}{lmfm}(object, correlation = FALSE, \dots)
}

\arguments{
  \item{object}{an \code{lmfm} object.}

  \item{correlation}{a logical value.  If TRUE, the correlation matrices for the coefficients are included in the summaries.}

  \item{\dots}{additional arguments required by the generic \code{\link{summary}} function.}
}

\value{
a list with class summary.lmfm whose elements are summaries of each model in \code{object}.  
}

\examples{
data(stackloss)
m1 <- lm(stack.loss ~ ., data = stackloss)
m2 <- lm(stack.loss ~ . - Acid.Conc., data = stackloss)

fm <- fit.models(m1, m2)
print(fm.sum <- summary(fm))
}

\keyword{methods}
\keyword{regression}


