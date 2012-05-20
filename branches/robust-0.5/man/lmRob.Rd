\name{lmRob}
\alias{lmRob}

\title{High Breakdown and High Efficiency Robust Linear Regression }

\description{
  Performs a robust linear regression with high breakdown point and high
  efficiency regression.
}

\usage{
lmRob(formula, data, weights, subset, na.action,
      model = TRUE, x = FALSE, y = FALSE, contrasts = NULL,
      nrep = NULL, control = lmRob.control(...), ...)
}

\arguments{
\item{formula}{a \code{formula} object, with the response on the left of a ~ operator, and the terms, separated by \code{+} operators, on the right.}

\item{data}{a \code{data.frame} in which to interpret the variables named in the \code{formula}, or in the \code{subset} and the \code{weights} argument. If this is missing, then the variables in the \code{formula} should be on
the search list.  This may also be a single number to handle some special cases - see below for details.}

\item{weights}{vector of observation weights; if supplied, the algorithm fits to minimize the sum of a function of the square root of the weights multiplied into the residuals.  The length of \code{weights} must be the same as
the number of observations. The weights must be nonnegative and it is strongly recommended that they be strictly positive, since zero weights are ambiguous, compared to use of the \code{subset} argument.}

\item{subset}{expression saying which subset of the rows of the data should be used in the fit.  This can be a logical vector (which is replicated to have length equal to the number of observations), or a numeric vector indicating which observation numbers are to be included, or a character vector of the row names to be included.  All observations are included by default.}

\item{na.action}{a function to filter missing data. This is applied to the \code{model.frame} after any \code{subset} argument has been used.  The default (with \code{na.fail}) is to create an error if any missing values are found.  A possible alternative is \code{na.exclude}, which deletes observations that contain one or more missing values.}

\item{model}{a logical flag: if \code{TRUE}, the model frame is returned in component \code{model}.}

\item{x}{a logical flag: if \code{TRUE}, the model matrix is returned in component \code{x}.}

\item{y}{a logical flag: if \code{TRUE}, the response is returned in component \code{y}.}

\item{contrasts}{a list giving contrasts for some or all of the factors appearing in the model formula.  The elements of the list should have the same name as the variable and should be either a contrast matrix (specifically, any full-rank matrix with as many rows as there are levels in the factor), or else a function to compute such a matrix given the number of levels.}

%% MM:  nrep should *really* be part of 'control', and hence
%%      lmRob.control() !
\item{nrep}{the number of random subsamples to be drawn. If \code{"Exhaustive"} resampling is being used, the value of \code{nrep} is ignored.}

\item{control}{a list of control parameters to be used in the numerical algorithms. See \code{\link{lmRob.control}} for the possible control parameters and their default settings.}

\item{\dots}{additional arguments are passed to the ccontrol functions.}
}

\value{
a list describing the regression.  Note that the solution returned here is an approximation to the true solution based upon a random algorithm (except when \code{"Exhaustive"} resampling is chosen).  Hence you will get (slightly) different answers each time if you make the same call with a different seed.  See \code{\link{lmRob.control}} for how to set the seed, and see \code{\link{lmRob.object}} for a complete description of the object returned.
}

\details{
By default, the \code{lmRob} function automatically chooses an appropriate algorithm to compute a final robust estimate with high breakdown point and high efficiency.  The final robust estimate is computed based on an initial estimate with high breakdown point.  For the initial estimation, the alternate M-S estimate is used if there are any factor variables in the predictor matrix, and an S-estimate is used otherwise.  To compute the S-estimate, a random resampling or a fast procedure is used unless the data set is small, in which case exhaustive resampling is employed.   See \code{\link{lmRob.control}} for how to choose between the different algorithms.
}

\section{References}{
  Gervini, D., and Yohai, V. J. (1999).
  A class of robust and fully efficient regression estimates;
  mimeo, Universidad de Buenos Aires.

  Marazzi, A. (1993).
  \emph{Algorithms, routines, and S functions for robust statistics}.
  Wadsworth & Brooks/Cole, Pacific Grove, CA.

%   Maronna, R. A., and Yohai, V. J. (1999).
%   Robust regression with both continuous and categorical predictors;
%   mimeo, Universidad de Buenos Aires.

  Maronna, R. A., and Yohai, V. J. (2000).
  Robust regression with both continuous and categorical predictors.
  \emph{Journal of Statistical Planning and Inference} \bold{89}, 197--214.

  Pena, D., and Yohai, V. (1999).
  A Fast Procedure for Outlier Diagnostics in Large Regression Problems.
  \emph{Journal of the American Statistical Association} \bold{94}, 434--445.

  Yohai, V. (1988).
  High breakdown-point and high efficiency estimates for regression.
  \emph{Annals of Statistics} \bold{15}, 642--665.

  Yohai, V., Stahel, W. A., and Zamar, R. H. (1991).
  A procedure for robust estimation and inference in linear regression; in
  Stahel, W. A. and Weisberg, S. W., Eds.,
  \emph{Directions in robust statistics and diagnostics, Part II}.
  Springer-Verlag.
}

\seealso{
\code{\link{lmRob.control}},
\code{\link{lmRob.object}}.
}

\examples{
data(stack.dat)
stack.rob <- lmRob(Loss ~ ., data = stack.dat)
}

\keyword{robust}
\keyword{regression}
\keyword{models}


