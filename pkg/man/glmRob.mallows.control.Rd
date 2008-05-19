\name{glmRob.mallows.control}
\alias{glmRob.mallows.control}

\title{
Control for Mallows-type Robust GLM Estimator
}

\description{
Allows users to set parameters for \code{glmRob}.
}

\usage{
glmRob.mallows.control(wt.fn = wt.carroll, wt.tuning = 8, ...)
}

\arguments{

\item{wt.fn}{a weight function that might depend on a tuning constant. This function will be evaluated at the square root of the robust Mahalanobis distances of the covariates divided by their dimension.}

\item{wt.tuning}{a tuning constant for \code{wt.fn}.}

\item{\dots}{additional arguments are ignored.}
}

\value{
a list is returned, consisting of these parameters packaged to be used by \code{glmRob()}. The values for \code{glmRob.mallows.control()} can be supplied directly in a call to \code{glmRob()}. These values are filtered through \code{glmRob.mallows.control()} inside \code{glmRob()}. 
}

\seealso{
\code{\link{glmRob}}. 
}

\keyword{robust}
\keyword{regression}


