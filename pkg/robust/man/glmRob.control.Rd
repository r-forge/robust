\name{glmRob.control}
\alias{glmRob.control}

\title{glmRob Control Parameters}

\description{
  Generates a list of control parameters for glmRob. The main purpose of this function is to implement the default behaviour for glmRob. Use the functions listed in the See Also section to generate control lists for the different robust estimators.
}

\usage{
glmRob.control(method, ...)
}

\arguments{
  \item{method}{a character vector specifying which extimator the control parameters should be generated for. The choices are \code{"cubif"}, \code{"mallows"}, and \code{"misclass"}.}

  \item{\dots}{additional arguments are included in the control (if appropriate for the estimator specified by \code{method}).}
}

\value{
  a list of control parameters appropriate for the fitting method specified by the \code{method} argument. 
}


\seealso{
  \code{\link{glmRob.cubif.control}},
  \code{\link{glmRob.mallows.control}},
  \code{\link{glmRob.misclass.control}}.
}

\keyword{robust}
\keyword{regression}


