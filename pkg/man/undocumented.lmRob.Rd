\name{undocumented.lmRob}

\alias{lmRob.fit}
\alias{lmRob.wfit}
\alias{lmRob.const}
\alias{lmRob.eff0}
\alias{lmRob.effad}
\alias{lmRob.lar}
\alias{lmRob.wm}

\title{Undocumented lmRob Helper Functions}

\description{
These functions are used by \code{\link{lmRob}} and its methods.  They are not intended to be called directly by users and are not guaranteed to be included in future versions of the Robust Library.
}

\usage{
lmRob.fit(x, y, x1.idx = NULL, nrep = NULL, robust.control = NULL, ...)
lmRob.wfit(x, y, w, x1.idx = NULL, nrep = NULL, robust.control = NULL, ...)
lmRob.const(eff, ipsi = 1)
lmRob.eff0(mu = 1, itype = 1, iucv = 1, iwww = 1, ialfa = 1, sigmx = 1, upper = 10, til = 1e-04, maxit = 150, tol = 1e-05, epmach = .Machine$double.eps, uflow = .Machine$double.xmin, ta = 0, tb = 0, tc = 0, ipsi = 3)
lmRob.effad(eff)
lmRob.lar(x, y, tol = 1e-06)
lmRob.wm(x, y, coeff0, ucov0, scale0, itype = 1, isigma = -1, ipsi = 1, xk = 0.9440982, beta = 0.2661, wgt = y, tlo = 1e-04, tua = 1.5e-06, mxr = 50)
}

\arguments{
\item{x}{x}
\item{y}{y}
\item{x1.idx}{x1.idx}
\item{nrep}{nrep}
\item{robust.control}{robust.control}
\item{...}{...}
\item{w}{w}
\item{eff}{eff}
\item{ipsi}{ipsi}
\item{mu}{mu}
\item{itype}{itype}
\item{iucv}{iucv}
\item{iwww}{iwww}
\item{ialfa}{ialfa}
\item{sigmx}{sigmx}
\item{upper}{upper}
\item{til}{til}
\item{maxit}{maxit}
\item{tol}{tol}
\item{epmach}{epmach}
\item{uflow}{uflow}
\item{ta}{ta}
\item{tb}{tb}
\item{tc}{tc}
\item{coeff0}{coeff0}
\item{ucov0}{ucov0}
\item{scale0}{scale0}
\item{isigma}{isigma}
\item{xk}{xk}
\item{beta}{beta}
\item{wgt}{wgt}
\item{tlo}{tlo}
\item{tua}{tua}
\item{mxr}{mxr}
}

\seealso{\code{\link{lmRob}}}

\keyword{robust}

