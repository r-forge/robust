\name{glmRob.misclass}
\alias{glmRob.misclass}

\title{Consistent Misclassification Estimator}

\description{
Computes the consistent misclassification estimate provided in \code{\link{glmRob}}.
}

\usage{
glmRob.misclass(x, y, control, offset, null.dev, family, Terms)
}

\arguments{
  \item{x}{model matrix.}
  \item{y}{response.}
  \item{control}{control parameters.}
  \item{offset}{offset.}
  \item{null.dev}{a logical value.}
  \item{family}{a binomial family object.}
  \item{Terms}{the Terms object computed in glmRob.}
}

\value{
a list similar to \code{\link{glmRob.object}}.
}

\seealso{\code{\link{glmRob}}}

\examples{
data(leuk.dat)

glmRob(y ~ ag + wbc, data = leuk.dat, family = binomial(), method = 'misclass')
}

\keyword{robust}
\keyword{regression}


