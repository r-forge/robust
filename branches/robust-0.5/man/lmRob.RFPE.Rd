\name{lmRob.RFPE}
\alias{lmRob.RFPE}

\title{Robust Final Prediction Errors }

\description{
Computes the robust Final Prediction Errors (FPE) for a robust regression fit using M-estimates.
}

\usage{
lmRob.RFPE(object, scale = NULL)
}

\arguments{
\item{object}{an lmRob object.}

\item{scale}{a numeric value specifying the scale estimate used to compute the robust FPE.  Usually this should
be the scale estimate from an encompassing model.  If \code{NULL}, the scale estimate in \code{object} is used.}
}

\value{
a single numeric value giving the robust final prediction error.
}

\seealso{
\code{\link{lmRob}},  
\code{\link{step.lmRob}},  
\code{\link{drop1.lmRob}}.
}

\examples{
data(stack.dat)
stack.rob <- lmRob(Loss ~ ., data = stack.dat)
lmRob.RFPE(stack.rob)
}

\keyword{robust}
\keyword{regression}


