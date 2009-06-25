\name{glmRob.cubif.null}
\alias{glmRob.cubif.null}

\title{glmRob CUBIF Null Deviance}

\description{
  Computes the null deviance in the CUBIF
  (\bold{c}onditionally \bold{u}nbiased \bold{b}ounded \bold{i}n\bold{f}luence)
  method.
}

\usage{
glmRob.cubif.null(x, y, ni, offset, ics, family,
                  control, robust.cov.control)
}

\arguments{
  \item{x}{model matrix.}
  \item{y}{response.}
  \item{ni}{ni.}
  \item{offset}{offset.}
  \item{ics}{ics.}
  \item{family}{a family object.}
  \item{control}{control parameters.}
  \item{robust.cov.control}{control parameters.}
}

\value{
  a numeric value containing the null deviance.
}

\seealso{
  \code{\link{glmRob.cubif}}
}

\keyword{robust}
\keyword{regression}


