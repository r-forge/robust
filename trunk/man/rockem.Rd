\name{rockem}
\alias{rockem}

\title{
  Contrained M-estimation
}

\description{
  Compute a robust estimate of location and scatter using Rocke's contrained M-estimator.
}

\usage{
  rockem(x, control)
}

\arguments{
  \item{x}{a numeric matrix contraining the data.}

  \item{control}{a list of control parameters.  The utility function \code{covRob.control} creates a list of the control parameters and their default values.  See details for the required control parameters and their default values.}
}

\value{
  a list with the following components:

  \item{call}{an image of the call that produced the object with all the arguments named.}

  \item{cov}{a numeric matrix containing the constrained M-estimate of the covariance/correlation matrix.}

   \item{center}{a numeric vector containing the constrained M-estimate of the location vector.}

  \item{raw.cov}{a numeric matrix containing the initial robust estimate of the covariance/correlation matrix.}

  \item{raw.center}{a numeric vector containing the initial robust estimate of the location vector.}
}

\details{
This function is called by the high-level function \code{covRob} when the M-estimator is specified (via the optional argument \code{estim = "m"}).  It may also be of interest to power users who want to compute a constrained m-estimate with a minimum of fuss.

The required control parameters and their default values are: 

  \describe{
    \item{\code{quan = 0.5}}{an integer value giving the number of observations whose covariance determinant will be minimized.}

    \item{\code{ntrial = 500}}{a positive integer specifying the number of random trial subsamples that are drawn for large datasets.}
  }

The control parameters \code{quan} and \code{ntrial} are passed to the function \code{\link{fastmcd}} which computes the initial robust estimate.  The remaining parameters affect the constrained m-estimator.

  \describe{
    \item{\code{r = 0.45}}{a numeric value between 0 and 0.5 giving the desired breakdown point.}

    \item{\code{alpha = 0.05}}{a numeric value between 0 and 1 specifying the fraction of points receiving zero weight.}

    \item{\code{tau = 1e-06}}{a positive numeric value specifying the tolerance used to determine the singularity of the estimated scatter matrix.}

    \item{\code{tol = 1e-03}}{a positive numeric value giving the relative precision for the solution of the M-estimate.}

    \item{\code{maxit = 150}}{a positive integer specifying the maximum number of m iterations.}
  }
}

\section{References}{
David M. Rocke (1996).  Robustness properties of S-estimators of multivariate location and shape in high dimension.  \emph{Annals of Statistics}, Vol. 24, No. 3, 1327-1345.
}

\seealso{
  \code{\link{covRob}},
  \code{\link{covRob.control}}.
}

\examples{
  data(woodmod.dat)
  X <- as.matrix(woodmod.dat)
  m.control <- covRob.control("m")
  rockem(X, m.control)
}

%\keyword{librobust}
\keyword{multivariate}
\keyword{robust}

