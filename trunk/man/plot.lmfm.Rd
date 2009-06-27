\name{plot.lmfm}
\alias{plot.lmfm}

\title{Comparison Plots for Linear Regression Models}

\description{
Creates a set plots useful for comparing the fitted linear models in an lmfm object.  The plot options are (2) Normal QQ-Plot of Residuals, (3) Estimated Kernel Density of Residuals, (4) Robust Residuals vs Robust Distances, (5) Residuals vs Fitted Values, (6) Sqrt of abs(Residuals) vs Fitted Values, (7) Response vs Fitted Values, (8) Standardized Residuals vs Index (Time), (9) Overlaid Normal QQ-Plot of Residuals, and (10) Overlaid Estimated Density of Residuals.  For simple linear regression models there is also the option to have a side-by-side plots of the the fit over a scatter plot of the data.
}

\usage{
\method{plot}{lmfm}(x, which.plots = "ask",  ...)
}

\arguments{
\item{x}{an lmfm object.}

\item{which.plots}{either "ask", "all", or an integer vector specifying which plots to draw.  If \code{which.plots} is an integer vector, use the plot numbers given in the description above (or in the "ask" menu).}

\item{...}{additional arguments are passed to the ploting subfunctions which are listed in the see also section.}
}

\value{
\code{x} is invisibly returned.
}

\section{Side Effects}{
The selected plots are drawn on a graphics device.
}

\details{
This function is a wrapper for the lmfm*Plot functions listed in the see also section.  For finer control you may wish to call these functions directly.
}

\section{References}{
Atkinson, A. C. (1985).  Plots, Transformations and Regression. New York: Oxford University Press.
}

\seealso{
\code{\link{plot}},
\code{\link{fit.models}},
\code{\link{lmfm2DRegPlot}},
\code{\link{lmfmResQQPlot}},
\code{\link{lmfmSqrtResVsFittedPlot}},
\code{\link{lmfmOverlaidQQPlot}},
\code{\link{lmfmResVsFittedPlot}},
\code{\link{lmfmStdResPlot}},
\code{\link{lmfmOverlaidResDenPlot}},
\code{\link{lmfmRespVsFittedPlot}},
\code{\link{lmfmResKernDenPlot}},
\code{\link{lmfmSRvsRDPlot}}.
}

\examples{
data(stack.dat)
stack.fm <- fit.models(list(Robust = "lmRob", LS = "lm"), Loss ~ ., data = stack.dat)
plot(stack.fm, which.plots = 2)
}

\keyword{methods}
\keyword{hplot}


