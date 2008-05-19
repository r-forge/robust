\name{glmRob.cubif.Ini}
\alias{glmRob.cubif.Ini}

\title{CUBIF Initialization}

\description{
Compute the initial coefficient estimates used in the 
}

\usage{
glmRob.cubif.Ini(X, y, ni, icase, offset, b, zmin, epsilon, ia1, control)
}

\arguments{
  \item{X}{model matrix.}
  \item{y}{response.}
  \item{ni}{ni.}
  \item{icase}{family indicator.}
  \item{offset}{offset.}
  \item{b}{tuning parameter.}
  \item{zmin}{tuning parameter.}
  \item{epsilon}{tolerance.}
  \item{ia1}{ia1.}
  \item{control}{control parameters.}
}

\value{
a list containing the initial coefficient estimates.
}

\seealso{\code{\link{glmRob.cubif}}}

\keyword{robust}
\keyword{regression}


