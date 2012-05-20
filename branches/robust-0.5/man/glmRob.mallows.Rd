\name{glmRob.mallows}
\alias{glmRob.mallows}

\title{Mallows Type Estimator}

\description{
Computes the Mallows Type Estimator provided by glmRob.
}

\usage{
glmRob.mallows(x, y, control, offset, null.dev, family, Terms)
}

\arguments{
  \item{x}{model matrix}
  \item{y}{a numeric vector of Bernoulli responses.}
  \item{control}{control parameters.}
  \item{offset}{offset}
  \item{null.dev}{a logical value. If \code{TRUE} the null deviance is computed and stored.}
  \item{family}{a binomial family object.}
  \item{Terms}{the \code{Terms} object created in glmRob.}
}

\value{
a list similar to \code{\link{glmRob.object}}.
}

\seealso{\code{link{glmRob}}}

\examples{
data(mallows.dat)

glmRob(y ~ a + b + c, data = mallows.dat, family = binomial(), method = 'mallows')
}

\keyword{robust}
\keyword{regression}


