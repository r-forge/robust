\name{lmRob.control}

\alias{lmRob.control}

\title{
  Control Parameters for Robust Linear Regression
}

\description{
Allows the users to set values affecting the estimation procedure for
robust regression in \code{lmRob}.
}

\usage{
lmRob.control(tlo = 1e-4, tua = 1.5e-06, mxr = 50, mxf = 50, mxs = 50,
     tl = 1e-06, estim = "Final", initial.alg = "Auto", final.alg = "MM",
     seed = 1313, level = 0.1, efficiency = 0.9,
     weight = c("Optimal", "Optimal"), trace = TRUE)
}

\arguments{
\item{tlo}{
the relative tolerance in the iterative algorithms.
}

\item{tua}{
the tolerance used for the determination of pseudo-rank.
}

\item{mxr}{
the maximum number of iterations in the refinement step.
}

\item{mxf}{
the maximum number of iterations for computing final coefficient estimates.
}

\item{mxs}{
the maximum number of iterations for computing scale estimate.
}

\item{tl}{
the tolerance for scale denominators.  If a scale estimate becomes less than \code{tl}, the scale estimate is set equal to \code{tl}.
}

\item{estim}{
parameter that determines the type of estimator to be computed.  If \code{estim="Initial"}, only the initial estimates are computed; if \code{estim="Final"}, then final estimates are returned.
}

\item{initial.alg}{
parameter that determines the algorithm for initial estimates.  Valid choices are \code{"Auto"} for data-dependent
algorithm, \code{"Random"} for random resampling, \code{"Exhaustive"} for exhaustive resampling, \code{"Fast"} for fast procedure, and \code{"Genetic"} for genetic algorithm. By default, \code{lmRob} uses \code{"Auto"}.
}

\item{final.alg}{
parameter that determines the type of the final estimates. Valid choices are \code{"Adaptive"} for the robust efficient weighted least squares as proposed in Gervini and Yohai (1999), and \code{"MM"} for MM-estimate as proposed in Yohai, Stahel and Zamar (1991). By default, \code{lmRob} uses \code{"MM"}.
}

\item{seed}{
seed parameter used in the random sampling and genetic algorithm for the computation of initial estimates.
}

\item{weight}{
a character vector that determines the type of loss functions to be used.  The first determines the loss function used for the initial estimates, and the second determines the loss function used for the final M-estimates.  Valid choices are \code{"Optimal"} and \code{"Bisquare"}.
}

\item{level}{
the level of significance of the test for bias of the final MM-estimates, if desired later on.
}

\item{efficiency}{
the asymptotic efficiency of the final estimate.
}

\item{trace}{
a logical flag: if \code{TRUE}, the remaining computing time will be printed.
}
}

\value{
a list containing the values used for each of the control parameters.
}

\seealso{
\code{\link{lmRob}}.
}

\examples{
data(stack.dat)
my.control <- lmRob.control(weight=c("Bisquare","Optimal"))
stack.bo <- lmRob(Loss ~ ., data = stack.dat, control = my.control)
}

\keyword{robust}
\keyword{regression}


