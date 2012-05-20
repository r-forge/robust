\name{weight.funs}
\alias{psi.weight}
\alias{rho.weight}
\alias{psp.weight}
\alias{chi.weight}

\title{Weight Functions Psi, Rho, Chi}

\description{
  %% FIXME !
  These functions compute the weights used by lmRob and its associated methods.
}

\usage{
psi.weight(x, ips = 1, xk = 1.06)
rho.weight(x, ips = 1, xk = 1.06)
psp.weight(x, ips = 1, xk = 1.06)
chi.weight(x, ips = 1, xk = 1.06)
}

\arguments{
  \item{x}{a numeric vector.}

  \item{ips}{integer determining the weight function:
    \describe{
      \item{\code{ips = 1}}{"optimal"},
      \item{\code{ips = 2}}{rescaled bisquare},
      \item{\code{ips = 3}}{Huber},
      \item{\code{ips = 4}}{smoothed Huber},
      which is currently only available for \code{psi.*()} and its
      derivative \code{psp.*()}.
  }}
  \item{xk}{a numeric value specifying the tuning constant.}
}

\details{
  See the section \dQuote{Theoretical Details}, p. 58-59, in chapter 2 of
  \file{Robust.pdf}.
  %% Huber isn't there; and notably   "smoothed Huber" ain't
}

\value{
  a numeric vector, say \code{r} of the same length as \code{x},
  containing the function values \eqn{r_i = f(x_i)}{r[i] = f(x[i])}.
}

\examples{
x <- seq(-4,4, length=401)
f.x <- cbind(psi = psi.weight(x), psp = psp.weight(x),
             chi = chi.weight(x), rho = rho.weight(x))
es <- expression(psi(x), {psi*minute}(x), chi(x), rho(x))
leg <- as.expression(lapply(seq_along(es), function(i)
          substitute(C == E, list(C=colnames(f.x)[i], E=es[[i]]))))
matplot(x, f.x, type = "l", lwd = 1.5,
        main = "psi.weight(.) etc -- 'optimal'")
abline(h = 0, v = 0, lwd = 2, col = "#D3D3D380") # opaque gray
legend("bottom", leg, inset = .01,
       lty = 1:4, col = 1:4, lwd = 1.5, bg = "#FFFFFFC0")
}
\keyword{robust}


