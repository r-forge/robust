\name{print.summary.lmfm}
\alias{print.summary.lmfm}

\title{Print Method for summary.lmfm Objects}

\description{
Displays a side-by-side comparison of the summaries in an lmfm object.
}

\usage{
\method{print}{summary.lmfm}(x, ...)
}

\arguments{
  \item{x}{a summary.lmfm object.}

  \item{\dots}{additional arguments required by the generic \code{\link{print}} function.}
}

\value{
\code{x} is invisibly returned.
}

\seealso{
\code{\link{summary.lmfm}}.
}


\examples{
data(stack.dat)
stack.fm <- fit.models(list(Robust = "lmRob", LS = "lm"), Loss ~ ., data = stack.dat)
stack.sum <- summary(stack.fm)
print.summary.lmfm(stack.sum)
print(stack.sum)
}

\keyword{methods}


