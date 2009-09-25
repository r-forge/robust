\name{glmRob.cubif}
\alias{glmRob.cubif}

\title{Robust GLM CUBIF Fitter}

\description{
  Robustly fit a generalized linear model using a
  \bold{c}onditionally \bold{u}nbiased \bold{b}ounded
  \bold{i}n\bold{f}luence (\dQuote{cubif}) estimator.  This function is
  called by the high-level function \code{\link{glmRob}} when
  \code{method = "cubif"} (the default) is specified.
}

\usage{
glmRob.cubif(x, y, intercept = FALSE, offset = 0,
        family = binomial(), null.dev = TRUE, control)
}

\arguments{
  \item{x}{a numeric model matrix.}
  \item{y}{either a numeric vector containing the response or, in the case of the binomial family, a two-column numeric matrix containing the number of successes and failures.}
  \item{intercept}{a logical value. If \code{TRUE} a column of ones is added to the design matrix.}
  \item{offset}{a numeric vector containing the offset.}
  \item{family}{a family object.}
  \item{null.dev}{a logical value. If \code{TRUE} the null deviance is computed.}
  \item{control}{a list of control parameters. See \code{\link{glmRob.cubif.control}}.}
}

\value{
  See \code{\link{glmRob.object}}.
}

\references{
  Kunsch, L., Stefanski L. and Carroll, R. (1989).
  Conditionally Unbiased Bounded-Influence Estimation in General
  Regression Models, with Applications to Generalized Linear Models.
  JASA \bold{84}, 460--466.

  Marazzi, A. (1993).
  Algorithms, routines and S functions for robust statistics.
  Wadsworth & Brooks/Cole, Pacific Grove, CA.
}

\seealso{
  \code{\link{glmRob}},
  \code{\link{glmRob.cubif.control}}.
}

\keyword{robust}
\keyword{regression}


