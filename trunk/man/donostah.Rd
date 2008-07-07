\name{donostah}
\alias{donostah}

\title{
  Donoho-Stahel Covariance Estimation
}

\description{
  Compute a robust estimate of location and scale using the Donoho-Stahel projection-bsed estimator.
}

\usage{
  donostah(x, control)
}

\arguments{
  \item{x}{a numeric matrix containing the data.}

  \item{control}{a list of control parameters. The utility function \code{\link{covRob.control}} creates a list of the control parameters and their default values. See details for the required control parameters and their default values.}
}

\value{
a list with the following components:

  \item{cov}{a numeric matrix containing the Stahel-Donoho estimate of the covariance/correlation matrix.}

  \item{center}{a numeric vector containing the Stahel-Donoho estimate of the location vector.}

  \item{dist}{a numeric vector containing the squared Mahalanobis distances.}
}

\details{
This function is called by the high-level function \code{\link{covRob}} when the Stahel-Donoho estimator is specified (via the optional argument \code{estim = "donostah"}).  It may also be of interest to power users who want to compute a Stahel-Donoho estimate with a minimum of fuss.

  \describe{
    \item{\code{nresamp = "auto"}}{a positive integer giving the number of resamples required; \code{nresamp} may not be reached if too many of the size \code{p} subsamples, chosen out of the observed vectors, are in a hyperplane.  If \code{nresamp = 0} all subsamples are taken.  The default \code{nresamp = "auto"} is calculated to provide a breakdown point of \code{eps} with probability \code{prob}.}

    \item{\code{maxres = "auto"}}{a positive integer specifying the maximum number of resamples to be performed including those that are discarded due to linearly dependent subsamples.  The default value \code{maxres = "auto"} is 2 times \code{nresamp}.}

    \item{\code{random.sample = FALSE}}{a logical flag.  If \code{FALSE} then the random seed is set so that the estimate is reproducible.}

    \item{\code{center = TRUE}}{a logical flag or numeric vector containing the location about which the covariance is to be taken.  If \code{center = TRUE} then a robust estimate of the center is computed; if \code{center = FALSE} then no centering takes place and the center is taken to be the zero vector.}

    \item{\code{tune = 0.95}}{a numeric value between 0 and 1 giving the fraction of the data to receive non-zero weight.}

    \item{\code{prob = 0.99}}{a numeric value between 0 and 1 specifying the probability of high breakdown point; used to compute \code{nresamp} when \code{nresamp = "auto"}.}

    \item{\code{eps = 0.5}}{a numeric value between 0 and 0.5 specifying the breakdown point; used to compute \code{nresamp} when \code{nresamp = "auto"}.}
  }
}

\section{References}{
R. A. Maronna and V. J. Yohai (1995) The Behavior of the Stahel-Donoho Robust Multivariate Estimator. \emph{Journal of the American Statistical Association} \bold{90} (429), 330--341.
}

\seealso{
  \code{\link{covRob}},
  \code{\link{covRob.control}}.
}

\examples{
  data(woodmod.dat)
  X <- as.matrix(woodmod.dat)
  ds.control <- covRob.control("donostah")
  donostah(X, ds.control)
}

%\keyword{librobust}
\keyword{multivariate}
\keyword{robust}

