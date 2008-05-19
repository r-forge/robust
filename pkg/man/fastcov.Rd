\name{fastcov}
\alias{fastcov}

\title{
  Orthogonalised Pariwise Covariance Estimation
}

\description{
  Compute a robust estimate of location and scatter using an orthogonalized pairwise estimator.
}

\usage{
  fastcov(x, control)
}

\arguments{
  \item{x}{a numeric matrix containing the data.}

    \item{control}{a list of control parameters.  The utility function \code{covRob.control} creates a list of the control parameters and their default values.  See details for the required control parameters and their default values.}
}

\value{
  a list with the following components:

  \item{call}{an image of the call that produced the object with all the arguments named.}

  \item{cov}{a numeric matrix containing the Stahel-Donoho estimate of the covariance/correlation matrix.}

  \item{center}{a numeric vector containing the Stahel-Donoho estimate of the location vector.}

  \item{raw.cov}{a numeric matrix containing the initial robust estimate of the covariance/correlation matrix.}

  \item{raw.center}{a numeric vector containing the initial robust estimate of the location vector.}
}

\details{
This function is called by the high-level function \code{\link{covRob}} when either the pairwiseGK or the pairwiseQC estimator is specified (via the optional arguments \code{estim = "pairwisegk"} or \code{estim = "pairwiseqc"}).  It may also be of interest to power users who want to compute a pairwise estimate with a minimum of fuss.

Presently the only control parameter is the name of the pairwise estimator, either "pairwisegk" or "pairwiseqc".
}

\section{References}{
Alqallaf, F. A. (2003).  A new contamination model for robust estimation with large high-dimensional datasets.  Ph.D. Thesis.  \url{http://hajek.stat.ubc.ca/~ruben/website/Fatemah_thesis.pdf}

Maronna, R. A. and Zamar, R. H. (2002).  Robust estimates of location and dispersion for high-dimensional datasets.  \emph{Technometrics}, 44, 307-317.
}

\seealso{
  \code{\link{covRob}},
  \code{\link{covRob.control}}.
}

\examples{
  data(woodmod.dat)
  X <- as.matrix(woodmod.dat)
  qc.control <- covRob.control("pairwiseqc")
  fastcov(X, qc.control)

  gk.control <- covRob.control("pairwisegk")
  fastcov(X, gk.control)
}

%\keyword{librobust}
\keyword{multivariate}
\keyword{robust}

