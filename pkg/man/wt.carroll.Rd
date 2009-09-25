\name{wt.carroll}
\alias{wt.carroll}
\alias{wt.huber}

\title{Weight Functions}

\description{
  Compute various weights.
}

\usage{
wt.carroll(u, b)
wt.huber(u, const = 1.345)
}

\arguments{
  \item{u}{a numeric vector.}
  \item{b}{a single numeric value specifying the parameter b.}
  \item{const}{a tuning constant.}
}

\value{
a numeric vector containing the weights.
}
\seealso{
  \code{\link{psi.weight}}
}

\keyword{robust}


