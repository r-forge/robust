\name{qqPlot.fdfm}
\alias{qqPlot.fdfm}

\title{Comparison Quantile-Quantile Plot}

\description{
Side-by-side quantile-quantile plots of the sample versus estimated quantiles.
}

\usage{
qqPlot.fdfm(x, qqline = TRUE, ...)
}

\arguments{
  \item{x}{an \code{fdfm} object.}

  \item{qqline}{a logical value. If \code{TRUE} a qqline is included in each panel.}

  \item{\dots}{additional arguments are passed to \code{xyplot}.}
}

\examples{
  data(los, package = "robustbase")
  los.fm <- fit.models(c(Robust = "fitdstnRob", MLE = "fitdstn"),
                         x = los, densfun = "gamma")
  qqPlot.fdfm(los.fm, xlab = "x-axis label", ylab = "y-axis label",
              main = "Plot Title", pch = 4, col = "purple")
}

\keyword{hplot}


