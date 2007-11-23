\name{glmRob.Initial.LMS}
\alias{glmRob.Initial.LMS}

\title{Initial LMS Coefficient Estimates for CUBIF Method}

\description{
Computes initial parameter estimates for the CUBIF method using the least median of squares algorithm.
}

\usage{
glmRob.Initial.LMS(X, y, ni, dist, offset, icase)
}

\arguments{
  \item{X}{model matrix.}
  \item{y}{response.}
  \item{ni}{ni.}
  \item{dist}{distances.}
  \item{offset}{offset.}
  \item{icase}{family indicator.}
}

\value{
a list containing the initial parameter and scale estimates.
}

\seealso{
\code{\link{glmRob.cubif}}
}

\keyword{robust}
\keyword{regression}


