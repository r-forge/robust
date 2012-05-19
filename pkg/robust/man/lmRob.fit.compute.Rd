\name{lmRob.fit.compute}
\alias{lmRob.fit.compute}

\title{Fit a Robust Linear Model}

\description{
Fits a robust linear model with high breakdown point and high efficiency estimates.  This is used by \code{\link{lmRob}}, but not supposed to be called by the users directly.  
}

\usage{
lmRob.fit.compute(x, y, x1.idx = NULL, nrep = NULL, robust.control = NULL, ...)
}

\arguments{
\item{x}{a numeric matrix containing the design matrix. }

\item{y}{a numeric vector containing the linear model response.}

\item{x1.idx}{a numeric vector containing the indices of columns of the design matrix arising from the coding of factor variables.}

\item{nrep}{the number of random subsamples to be drawn.  If \code{"Exhaustive"} resampling is being used, the value of \code{nrep} is ignored.}

\item{robust.control}{a list of control parameters to be used in the numerical algorithms. See \code{lmRob.control} for the possible control parameters and their default settings.}

\item{\dots}{additional arguments.}
}

\value{
an object of class \code{"lmRob"}.  See \code{\link{lmRob.object}} for a complete description of the object returned. 
}

\section{References}{
Gervini, D., and Yohai, V. J. (1999). A class of robust and fully efficient regression estimates, mimeo, Universidad de Buenos Aires. 

Marazzi, A. (1993).  
\emph{Algorithms, routines, and S functions for robust statistics}.  Wadsworth & Brooks/Cole, Pacific Grove, CA. 

Maronna, R. A., and Yohai, V. J. (1999).  Robust regression with both continuous and categorical predictors, mimeo, Universidad de Buenos Aires. 

Yohai, V. (1988). High breakdown-point and high efficiency estimates for regression, \emph{Annals of Statistics}, 15, 642-665. 

Yohai, V., Stahel, W. A., and Zamar, R. H. (1991). A procedure for robust estimation and inference in linear regression, in Stahel, W. A. and Weisberg, S. W., Eds., \emph{Directions in robust statistics and diagnostics, Part II}. Springer-Verlag. 
}

\seealso{
\code{\link{lmRob}},
\code{\link{lmRob.control}}.
}

\keyword{robust}
\keyword{regression}
\keyword{models}


