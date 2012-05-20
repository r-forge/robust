\name{plot.lmRob}
\alias{plot.lmRob}

\title{Diagnostic Regression Plots}

\description{
Creates a set plots useful for assessing a robustly fitted linear model.  The plot options are (2) Normal QQ-Plot of Residuals, (3) Estimated Kernel Density of Residuals, (4) Robust Residuals vs Robust Distances, (5) Residuals vs Fitted Values, (6) Sqrt of abs(Residuals) vs Fitted Values, (7) Response vs Fitted Values, (8) Standardized Residuals vs Index (Time), (9) Overlaid Normal QQ-Plot of Residuals, and (10) Overlaid Estimated Density of Residuals.  For simple linear regression models there is also the option to have a side-by-side plots of the the fit over a scatter plot of the data.
}

\usage{
\method{plot}{lmRob}(x, which.plots = c(5, 2, 6, 4), ...)
}

\arguments{
\item{x}{an lmRob object.}

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
This function casts the lmRob object as an lmfm object containing a single model.  The actual ploting is then done by the function \code{\link{plot.lmfm}}.
}

\section{References}{
Atkinson, A. C. (1985).  Plots, Transformations and Regression. New York: Oxford University Press.
}

\seealso{
\code{\link{plot}},
\code{\link{lmRob}},
\code{\link{plot.lmfm}}.
}

\examples{
data(stack.dat)
stack.rob <- lmRob(Loss ~ ., data = stack.dat)
plot(stack.rob, which.plots = 6)
}

\keyword{methods}
\keyword{hplot}


