\name{summary.lmRob}
\alias{summary.lmRob}

\title{Summarizing Robust Linear Model Fits}

\description{
Compute a summary of the robustly fitted linear model.
}

\usage{
\method{summary}{lmRob}(object, correlation = FALSE, bootstrap.se = FALSE, ...)
}

\arguments{
\item{object}{an lmRob object.}

\item{correlation}{a logical value.  If \code{TRUE} then the correlation matrix of the coefficients is included in the summary.}

\item{bootstrap.se}{a logical value.  If \code{TRUE} then bootstrap standard error estimates are included in the summary.}

\item{...}{additional arguments required by the generic \code{\link{summary}} function.}
}

\value{
The summary is returned in a list of class summary.lmRob and contains the following components: 

  \item{sigma}{a single numeric value containing the residual scale estimate.}

  \item{df}{a numeric vector of length 3 containing integer values: the rank of the model matrix, the residual degrees of freedom, and the number of coefficients in the model.}

  \item{cov.unscaled}{the unscaled covariance matrix; i.e, the matrix that, when multiplied by the estimate of the error variance, yields the estimated covariance matrix for the coefficients.}

  \item{correlation}{the correlation coefficient matrix for the coefficients in the model.}

  \item{...}{the remaining components are the same as the corresponding components in an \code{lmRob} object. Use the \code{\link{names}} function to obtain a list of the components.}
}

\examples{
data(stack.dat)
stack.rob <- lmRob(Loss ~ ., data = stack.dat) 
stack.sum <- summary(stack.rob)
stack.sum
stack.bse <- summary(stack.rob, bootstrap.se = TRUE)
stack.bse
}

\keyword{methods}
\keyword{robust}
\keyword{regression}


