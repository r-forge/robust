\name{print.summary.covfm}
\alias{print.summary.cov}
\alias{print.summary.covRob}
\alias{print.summary.covfm}

\title{Print Methods for Cov* Summary Results}

\description{
  The generic print method for objects of class "summary.cov",
  "summary.covRob", and "summary.covfm".
}

\usage{
\method{print}{summary.cov}(x, print.distance = TRUE, ...)
\method{print}{summary.covRob}(x, print.distance = TRUE, ...)
\method{print}{summary.covfm}(x, print.distance = FALSE, ...)
}

\arguments{
  \item{x}{an oject of class "summary.cov", "summary.covRob", or "summary.covfm".}

  \item{print.distance}{a logical flag.  If \code{print.distance = TRUE}
    the mahalanobis distances are printed as well.}

  \item{\dots}{additional arguments to be passed to the print functions,
    for example \code{digits}.}
}

\value{
  \code{x} is invisibly returned.
}

\section{Side Effects}{
  a description of the summary object is displayed in the console.
}

\seealso{
  \code{\link{print}}, \code{\link{summary}},
  \code{\link{ccov}}, \code{\link{covRob}},
  \code{\link{fit.models}}.
}

\examples{
  data(woodmod.dat)
  woodmod.cov <- ccov(woodmod.dat)
  woodmod.sc <- summary(woodmod.cov)
  print(woodmod.sc)

  woodmod.covRob <- covRob(woodmod.dat)
  woodmod.scr <- summary(woodmod.covRob)
  print(woodmod.scr, digits = 4)

  woodmod.fm <- fit.models(list(Robust = "covRob", Classical = "ccov"),
                           data = woodmod.dat)
  woodmod.sfm <- summary(woodmod.fm)
  print(woodmod.sfm, print.distances = TRUE)
}

%\keyword{librobust}
\keyword{methods}

