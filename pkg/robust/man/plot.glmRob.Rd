\name{plot.glmRob}
\alias{plot.glmRob}

\title{Diagnostic Regression Plots}

\description{
Creates a set plots useful for assessing a robustly fitted generalized linear model.  The plot options are (2) Deviance Residuals vs. Predicted Values, (3) Response vs. Predicted Values, (4) Normal QQ Plot of Pearson Residuals, (5) QQ Plot of Deviance Residuals, (6) Standardized Deviance Residuals vs. Robust Distances, (7) Standardized Deviance Residuals vs. Index (Time), and (8) Sqrt of abs(Deviance Residuals) vs. Fitted Values.
}

\usage{
\method{plot}{glmRob}(x, which.plots = c(2, 5, 7, 6), ...)
}

\arguments{
\item{x}{a glmRob object.}

\item{which.plots}{either "ask", "all", or an integer vector specifying which plots to draw.  If \code{which.plots} is an integer vector, use the plot numbers given in the description above (or in the "ask" menu).}

\item{...}{additional arguments are pass to the ploting subfunctions which are listed in the see also section.}
}

\value{
\code{x} is invisibly returned.
}

\section{Side Effects}{
The selected plots are drawn on a graphics device.
}

\details{
This function casts the glmRob object as an glmfm object containing a single model.  The actual ploting is then done by the function \code{\link{plot.glmfm}}.
}

\section{References}{
Atkinson, A. C. (1985).  Plots, Transformations and Regression. New York: Oxford University Press.
}

\seealso{
\code{\link{plot}},
\code{\link{glmRob}},
\code{\link{plot.glmfm}}.
}

\keyword{methods}
\keyword{hplot}

