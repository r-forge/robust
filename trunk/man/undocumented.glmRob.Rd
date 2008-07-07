\name{undocumented.glmRob}

\alias{glmRob.gfedca}
\alias{glmRob.gicstp}
\alias{glmRob.gintac}
\alias{glmRob.glmdev}
\alias{glmRob.gymain}
\alias{glmRob.gytstp}
\alias{glmRob.ktaskw}

\title{Undocumented glmRob Helper Functions}

\description{
These functions are used by \code{\link{glmRob}} and its methods.  They are not intended to be called directly by users and are not guaranteed to be included in future versions of the Robust Library.
}

\usage{
glmRob.gfedca(vtheta, ci, wa, ni, oi = 0, icase) 
glmRob.gicstp(icase, ialg, ni, vtheta, ai, oi, tol, maxit)
glmRob.gintac(x, y, ni, oi, icase, maxtt, maxta, tolt, tola, b, c) 
glmRob.glmdev(y, ni, ci, wa, vtheta, offset = 0, icase = 0)
glmRob.gymain(x, y, ni, cov, a, theta, oi = 0, b,
  gam, tau, icase, iugl, iopt, ialg, icnvt, icnva, maxit, maxtt,
  maxta, maxtc, tol, tolt, tola, tolc, trc = FALSE) 
glmRob.gytstp(x, y, ci, theta, wa, cov, ni, offset, tol, icase, maxit)
glmRob.ktaskw(x, d, e, tau, ia, f, f1, iainv, a)
}

\arguments{
\item{vtheta}{vtheta}
\item{ci}{ci}
\item{wa}{wa}
\item{ni}{ni}
\item{oi}{oi}
\item{icase}{icase}
\item{ialg}{ialg}
\item{ai}{ai}
\item{tol}{tol}
\item{maxit}{maxit}
\item{x}{x}
\item{y}{y}
\item{maxtt}{maxtt}
\item{maxta}{maxta}
\item{tolt}{tolt}
\item{tola}{tola}
\item{b}{b}
\item{c}{c}
\item{offset}{offset}
\item{cov}{cov}
\item{a}{a}
\item{theta}{theta}
\item{gam}{gam}
\item{tau}{tau}
\item{iugl}{iugl}
\item{iopt}{iopt}
\item{icnvt}{icnvt}
\item{icnva}{icnva}
\item{maxtc}{maxtc}
\item{tolc}{tolc}
\item{trc}{trc}
\item{d}{d}
\item{e}{e}
\item{ia}{ia}
\item{f}{f}
\item{f1}{f1}
\item{iainv}{iainv}
}

\seealso{\code{\link{glmRob}}}

\keyword{robust}

