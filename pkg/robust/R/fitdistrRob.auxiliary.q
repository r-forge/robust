Tabgamma <- function(b1 = 1.5, b2 = 1.7, alpha1 = 0.5, alpha2 = 20.5,
                     k = 101, A = c(0, 0, 0), maxta = 1, maxtc = 1,
                     maxit = 100, til = 0.001, tol = 0.001)
{
  tab <- matrix(0.0, nrow = k, ncol = 5)
  storage.mode(tab) <- "double"

  f.res <- .Fortran("rlcretabi",
                    b1 = as.double(b1),
                    b2 = as.double(b2),
                    kk = as.integer(k),
                    la = as.integer(2),
                    a = as.double(A),
                    maxta = as.integer(maxta),
                    maxtc = as.integer(maxtc),
                    maxit = as.integer(maxit),
                    til = as.double(til),
                    tol = as.double(tol),
                    alpha1 = as.double(alpha1),
                    alpha2 = as.double(alpha2),
                    monit = as.integer(0),
                    tab = tab,
                    tpar = double(6),
                    PACKAGE = "robust")

  tab <- f.res$tab
  dimnames(tab) <- list(NULL, c("c1", "c2", "a11", "a21", "a22"))
  tab
}


S.Theta.gamma <- function(alF, sigF, u = 0.99, beta = 0.4, gam = 0.4)
{
  if(abs(beta - 0.5) <= 1e-4) {

    # Theta <- S.Theta.D.g(alF,sigF,u)    

    mu1F <- alF
    muF <- alF * sigF

    # S.med.g(alF,sigF)
    medF <- sigF * qgamma(0.5, alF)

    # S.mad.g(alF,sigF)
    madF <- sigF * S.qad1.g(alF, 0.5, 0.5)$qad1

    # S.med.g(alF,1)
    M1F <- qgamma(0.5, alF)

    # S.mad.g(alF,1)
    D1F <- S.qad1.g(alF, 0.5, 0.5)$qad1

    # S.K1.g(M1F,alF)/sigF
    fmed <- dgamma(M1F, alF) / sigF

    # S.K1.g(M1F+D1F,alF)/sigF
    fMpD <- dgamma(M1F + D1F, alF) / sigF

    # S.K1.g(M1F-D1F,alF)/sigF
    fMmD <- dgamma(M1F - D1F, alF) / sigF

    # -S.K2.g(M1F,alF)/S.K1.g(M1F,alF)
    MpF <- -S.K2.g(M1F, alF) / dgamma(M1F, alF)
    A <- M1F + D1F
    B <- M1F - D1F

    #DpF  <- S.K2.g(B,alF)-S.K2.g(A,alF)-MpF*(S.K1.g(A,alF)-S.K1.g(B,alF))
    #DpF  <- DpF/(S.K1.g(A,alF)+S.K1.g(B,alF))
    DpF <- S.K2.g(B, alF) - S.K2.g(A, alF) - MpF * (dgamma(A, alF) - dgamma(B, alF))
    DpF <- DpF / (dgamma(A, alF) + dgamma(B, alF))

    z1 <- S.quql.g(u, alF, 1.0)
    qu1F <- z1$qu
    ql1F <- z1$ql

    zq <- S.quql.g(u, alF, sigF)
    quF <- zq$qu
    qlF <- zq$q

    #S.K1.g(quF/sigF,alF)/sigF
    fquF <- dgamma(quF / sigF, alF) / sigF

    #S.K1.g(qlF/sigF,alF)/sigF
    fqlF <- dgamma(qlF / sigF, alF) / sigF

    #S.K.g(ql1F,alF)
    FqlF <- pgamma(ql1F, alF)

    #S.H0.g(u,alF,sigF)
    H0 <- u
    qu1 <- qgamma(u, alF)

    #S.H1.g(u,alF,sigF)
    H1 <- alF * sigF * pgamma(qu1, alF + 1)

    #ql1 <- S.quql.g(u,alF,1)$ql
    #S.J0.g(u,alF,sigF)
    J0 <- pgamma(ql1F, alF)

    #ql1 <- S.quql.g(u,alF,1)$ql
    #S.J1.g(u,alF,sigF)
    J1 <- alF * sigF * pgamma(ql1F, alF + 1)

    #S.K.g(ql1F,alF)
    Kl1 <- pgamma(ql1F,alF)

    #S.K1.g(ql1F,alF)
    K1l1 <- dgamma(ql1F, alF)

    #S.K1.g(qu1F,alF)
    K1u1 <- dgamma(qu1F, alF)
    K2u1 <- S.K2.g(qu1F, alF)
    K2l1 <- S.K2.g(ql1F, alF)

    #S.G1.g(qu1F,alF)
    G1u1 <- qu1F * dgamma(qu1F, alF)

    #S.G1.g(ql1F,alF)
    G1l1 <- ql1F * dgamma(ql1F, alF)
    G2u1 <- S.G2.g(qu1F, alF)
    G2l1 <- S.G2.g(ql1F, alF)

    Theta <- list(iopt = 2, u = u, mu1F = mu1F, muF = muF, qu1F = qu1F,
                  quF = quF, ql1F = ql1F,qlF = qlF, fquF = fquF, fqlF = fqlF,
                  FqlF = FqlF, H0 = H0, H1 = H1, J0 = J0, J1 = J1, Kl1 = Kl1,
                  K1l1 = K1l1, K1u1 = K1u1, K2u1 = K2u1, K2l1 = K2l1,
                  G1u1 = G1u1, G1l1 = G1l1, G2u1 = G2u1, G2l1 = G2l1, alF = alF,
                  sigF = sigF, M1F = M1F, D1F = D1F, MpF = MpF, DpF = DpF,
                  medF = medF, madF = madF, fmed = fmed, fMpD = fMpD,
                  fMmD = fMmD)
  }
  
  else {

    #Theta <- S.Theta.Dsm.g(alF,sigF,u,beta,gam)

    mu1F <- alF
    muF <- alF * sigF
    tmg <- .Fortran("rltrmng",
                    alpha = as.double(alF),
                    sigma = as.double(sigF),
                    beta = as.double(beta),
                    mf = double(1),
                    PACKAGE = "robust")

    mF <- tmg$mf 
    m1F <- mF/sigF

    #S.K1.g(mF/sigF,alF)/sigF
    fm <- dgamma(mF / sigF, alF) / sigF

    D2 <- S.qad1.g(alF, beta, gam)$qad1 * sigF
    D1 <- S.qad1.g(alF, beta, 1 - gam)$qad1 * sigF
    QGup1 <- D1 + mF
    QGlow1 <- -D1 + mF

    # S.K1.g(QGup1/sigF,alF)/sigF
    fQGup1 <- dgamma(QGup1 / sigF, alF) / sigF

    # S.K1.g(QGlow1/sigF,alF)/sigF
    fQGlow1 <- dgamma(QGlow1 / sigF, alF) / sigF

    QGup2 <- D2 + mF
    QGlow2 <- -D2 + mF

    # S.K1.g(QGup2/sigF,alF)/sigF
    fQGup2 <- dgamma(QGup2 / sigF, alF) / sigF

    # S.K1.g(QGlow2/sigF,alF)/sigF
    fQGlow2 <- dgamma(QGlow2 / sigF, alF) / sigF

    # S.G.g(QGup2/sigF, alF)*sigF
    A1 <- alF * pgamma(QGup2 / sigF, alF + 1) * sigF

    # S.G.g(QGlow2/sigF,alF)*sigF
    A2 <- alF * pgamma(QGlow2 / sigF, alF + 1) * sigF

    # S.G.g(QGup1/sigF, alF)*sigF
    A3 <- alF * pgamma(QGup1 / sigF, alF + 1) * sigF

    # S.G.g(QGlow1/sigF,alF)*sigF
    A4 <- alF * pgamma(QGlow1 / sigF, alF+1) * sigF

    # S.K.g(QGup2/sigF,alF)
    B1 <- pgamma(QGup2 / sigF, alF)

    # S.K.g(QGlow2/sigF,alF)
    B2 <- pgamma(QGlow2 / sigF, alF)

    # S.K.g(QGup1/sigF,alF)
    B3 <- pgamma(QGup1 / sigF, alF)

    # S.K.g(QGlow1/sigF,alF)
    B4 <- pgamma(QGlow1 / sigF, alF)

    sF <- ((A1+A2-A3-A4) - mF*(B1+B2-B3-B4)) / (1 - 2*gam)
    uF <- qgamma(1 - beta, alF) * sigF
    lF <- qgamma(beta, alF) * sigF
    W2beta <- (1 - 2*beta) * mF + beta * lF + beta * uF

    # Derivatives of M
    upF <- -S.K2.g(uF / sigF, alF) / dgamma(uF / sigF, alF)
    lpF <- -S.K2.g(lF / sigF, alF) / dgamma(lF / sigF, alF)
    MpF <- (uF / sigF) * dgamma(uF / sigF, alF) * upF + S.G2.g(uF / sigF, alF) -
             ((lF / sigF) * dgamma(lF / sigF, alF) * lpF + S.G2.g(lF / sigF, alF))
    MpF <- MpF / (1 - 2*beta)

    # Derivative of D(1-gam) = D2
    A <- (mF + D2) / sigF
    B <- (mF - D2) / sigF
    D2pF <- S.K2.g(B, alF) - S.K2.g(A, alF) - MpF * (dgamma(A, alF) - dgamma(B, alF))
    D2pF <- D2pF / (dgamma(A, alF) + dgamma(B, alF))

    # Derivative of sF(1-gam)                        

    S2pF <- A * dgamma(A, alF) * (MpF + D2pF) + S.G2.g(A, alF) +
              B * dgamma(B, alF) * (MpF - D2pF) + S.G2.g(B, alF) -
                m1F * (dgamma(A, alF) * (MpF + D2pF) + S.K2.g(A, alF) +
                  dgamma(B, alF) * (MpF - D2pF) + S.K2.g(B, alF)) -
                    MpF*(B1+B2)

    # Derivative of D(gam) = D1
    A <- (mF + D1) / sigF
    B <- (mF - D1) / sigF
    D1pF <- S.K2.g(B, alF) - S.K2.g(A, alF) - MpF * (dgamma(A, alF) - dgamma(B, alF))
    D1pF <- D1pF / (dgamma(A, alF) + dgamma(B, alF))

    # Derivative of sF(gam)
    S1pF <- A * dgamma(A, alF) * (MpF + D1pF) + S.G2.g(A, alF) +
              B * dgamma(B, alF) * (MpF - D1pF) + S.G2.g(B, alF) -
                m1F * (dgamma(A, alF) * (MpF + D1pF) + S.K2.g(A, alF) +
                  dgamma(B, alF) * (MpF - D1pF) + S.K2.g(B, alF)) -
                    MpF*(B3+B4)

    # Derivative of S=S2-S1
    SpF <- (S2pF - S1pF) / (1 - 2*gam)

    z <- S.quql.g(u, alF, 1.0)
    qu1F <- z$qu
    ql1F <- z$ql

    z <- S.quql.g(u , alF, sigF)
    quF <- z$qu
    qlF <- z$ql

    fquF <- dgamma(quF / sigF, alF) / sigF
    fqlF <- dgamma(qlF / sigF, alF) / sigF

    # S.K.g(ql1F,alF)
    FqlF <- pgamma(ql1F, alF)

    # S.H0.g(u,alF,sigF)
    H0 <- u

    qu1 <- qgamma(u, alF) 

    # S.H1.g(u,alF,sigF)
    H1 <- alF * sigF * pgamma(qu1, alF + 1)

    # S.J0.g(u,alF,sigF)
    J0 <- pgamma(ql1F, alF)

    # S.J1.g(u,alF,sigF)
    J1 <- alF * sigF * pgamma(ql1F, alF + 1)

    # S.K.g(ql1F,alF)
    Kl1  <- pgamma(ql1F, alF)

    K1l1 <- dgamma(ql1F, alF)
    K1u1 <- dgamma(qu1F, alF)
    K2u1 <- S.K2.g(qu1F, alF)
    K2l1 <- S.K2.g(ql1F, alF)

    # S.G1.g(qu1F,alF)
    G1u1 <- qu1F * dgamma(qu1F, alF)

    # S.G1.g(ql1F,alF)
    G1l1 <- ql1F * dgamma(ql1F, alF)

    G2u1 <- S.G2.g(qu1F, alF)
    G2l1 <- S.G2.g(ql1F, alF)     

    Theta <- list(iopt = 1, alF = alF, sigF = sigF, beta = beta,
                  gam = gam, mF = mF, m1F = m1F, D1 = D1, D2 = D2,
                  sF = sF, s1F = 1, uF = uF, lF = lF, W2beta = W2beta,
                  A1 = A1, A2 = A2, A3 = A3, A4 = A4, B1 = B1, B2 = B2,
                  B3 = B3, B4 = B4, QGup1 = QGup1, QGlow1 = QGlow1,
                  QGup2 = QGup2, QGlow2 = QGlow2, fm = fm, fQGup1 = fQGup1,
                  fQGlow1 = fQGlow1, fQGup2 = fQGup2, fQGlow2 = fQGlow2,
                  MpF = MpF, SpF = SpF, u = u, mu1F = mu1F, muF = muF,
                  qu1F = qu1F, quF = quF, ql1F = ql1F, qlF = qlF, fquF = fquF,
                  fqlF = fqlF, FqlF = FqlF, H0 = H0, H1 = H1, J0 = J0, J1 = J1,
                  Kl1 = Kl1, K1l1 = K1l1, K1u1 = K1u1, K2u1 = K2u1, K2l1 = K2l1,
                  G1u1 = G1u1, G1l1 = G1l1, G2u1 = G2u1, G2l1 = G2l1,
                  D2pF = D2pF, D1pF = D1pF, S1pF = S1pF, S2pF = S2pF)
  }

  Theta
}


# S.TD.fun.g(1, ...)
S.quql.g <- function(u, alpha, sigma, tol = 1e-4)
{
  z <- .Fortran("rlquqldg",
                u = as.double(u),
                alpha = as.double(alpha),
                sigma = as.double(sigma),
                tol = as.double(tol),
                ql = double(1),
                qu = double(1),
                isol = integer(1),
                PACKAGE = "robust")

  list(ql = z$ql, qu = z$qu, ok = z$isol)
}


# S.TD.fun.g(2, ...)
S.qad1.g <- function(alpha, beta, gam, tol = 1e-4)
{
  z <- .Fortran("rlqad1dg",
                alpha = as.double(alpha),
                beta = as.double(beta),
                gam = as.double(gam),
                tol = as.double(tol),
                qad1 = double(1),
                isol = integer(1),
                PACKAGE = "robust")

  list(qad1 = z$qad1, ok = z$isol)
}


# S.TD.fun.g(3, ...)
S.Intlgam <- function(upper, alpha)
{
  .Fortran("rlsumlgm",
           hi = as.double(upper),
           alpha = as.double(alpha),
           gl = double(1),
           PACKAGE = "robust")$gl
}


# S.TD.fun.g(4, ...)
S.ingama <- function(x, p)
{
  .Fortran("rlingama",
           x = as.double(x),
           p = as.double(p),
           g = double(1),
           PACKAGE = "robust")$g
}


# S.TD.fun.g(5, ...)
S.digama <- function(s)
{
  .Fortran("rldigama",
           s = as.double(s),
           g = double(1),
           PACKAGE = "robust")$g
}


# S.TD.fun.g(6, ...)
S.K2.g <- function(t, alpha)
{
  S.Intlgam(t, alpha) - S.digama(alpha) * pgamma(t, alpha)
}


# S.TD.fun.g(7, ...)
S.G2.g <- function(t, alpha)
{
  u <- S.Intlgam(t, alpha + 1) - S.digama(alpha + 1) * pgamma(t, alpha + 1)
  pgamma(t, alpha + 1) + alpha*u
}


#Theta <- S.Theta.Dsm.l(alF,sigF,u,beta,gam)
S.Theta.lnorm <- function(alF, sigF, u = 0.99, beta = 0.4, gam = 0.4)
{
  iopt  <- 1

  if(abs(beta-0.5) <= 1e-4) {
    beta <- 0.4999
    gam <- 0.4999
  }

  #S.trmean.n(alF,beta)
  mF <- .Fortran("rltrmnn",
                  alpha = as.double(alF),
                  beta = as.double(beta),
                  mf = double(1.0),
                  PACKAGE = "robust")$mf

  #S.trmean.n(0,beta)
  m1F <- .Fortran("rltrmnn",
                  alpha = as.double(0.0),
                  beta = as.double(beta),
                  mf = double(1),
                  PACKAGE = "robust")$mf

  #S.K1.n(m1F)/sigF
  fm <- dnorm(m1F) / sigF

  #S.qad1.n(beta,gam)$qad1*sigF
  D2 <- .Fortran("rlqad1n",
                 beta = as.double(beta),
                 gam = as.double(gam),
                 tol = as.double(1e-4),
                 qad1 = double(1),
                 isol = integer(1),
                 PACKAGE = "robust")$qad1
  D2 <- D2 * sigF

  #S.qad1.n(beta,1-gam)$qad1*sigF
  D1 <- .Fortran("rlqad1n",
                 beta = as.double(beta),
                 gam = as.double(1.0 - gam),
                 tol = as.double(1e-4),
                 qad1 = double(1),
                 isol = integer(1),
                 PACKAGE = "robust")$qad1
  D1 <- D1 * sigF

  #S.trmadv.n(1,beta,gam)
  s1F <- .Fortran("rltrmadn",
                  sigma = as.double(1),
                  beta = as.double(beta),
                  gam = as.double(gam),
                  tol = as.double(1e-4),
                  sf = double(1),
                  isol = integer(1),
                  PACKAGE = "robust")$sf

  QGup1 <- D1 + mF
  QGlow1 <- -D1 + mF

  #S.K1.n((QGup1-alF)/sigF)/sigF
  fQGup1 <- dnorm((QGup1 - alF) / sigF) / sigF

  #S.K1.n((QGlow1-alF)/sigF)/sigF
  fQGlow1 <- dnorm((QGlow1 - alF) / sigF) / sigF

  QGup2 <- D2+mF
  QGlow2 <- -D2+mF

  #S.K1.n((QGup2-alF)/sigF)/sigF
  fQGup2 <- dnorm((QGup2 - alF) / sigF) / sigF

  #S.K1.n((QGlow2-alF)/sigF)/sigF
  fQGlow2 <- dnorm((QGlow2 - alF) / sigF) / sigF

  #S.G.n((QGup2-alF)/sigF)
  A1 <- (S.G.n((QGup2 - alF) / sigF, 0.0) + alF * pnorm((QGup2 - alF) / sigF) / sigF) * sigF
  A2 <- (S.G.n((QGlow2 - alF) / sigF, 0.0) + alF * pnorm((QGlow2 - alF) / sigF) / sigF) * sigF
  A3 <- (S.G.n((QGup1 - alF) / sigF, 0.0) + alF * pnorm((QGup1 - alF) / sigF) / sigF) * sigF
  A4 <- (S.G.n((QGlow1 - alF) / sigF) + alF * pnorm((QGlow1 - alF) / sigF) / sigF) * sigF

  #S.K.n((QGup2-alF)/sigF)
  B1 <- pnorm((QGup2 - alF) / sigF)

  #S.K.n((QGlow2-alF)/sigF)
  B2 <- pnorm((QGlow2 - alF) / sigF)

  #S.K.n((QGup1-alF)/sigF)
  B3 <- pnorm((QGup1 - alF) / sigF)

  #S.K.n((QGlow1-alF)/sigF)
  B4 <- pnorm((QGlow1 - alF) / sigF)

  #S.trmadv.n(sigF,beta,gam)
  sF <- .Fortran("rltrmadn",
                 sigma = as.double(sigF),
                 beta = as.double(beta),
                 gam = as.double(gam),
                 tol = as.double(1e-4),
                 sf = double(1),
                 isol = integer(1),
                 PACKAGE = "robust")$sf

  uF <- qnorm(1.0 - beta) * sigF + alF
  lF <- qnorm(beta) * sigF + alF
  W2beta <- (1.0 - 2.0 * beta) * mF + beta * lF + beta * uF

  #Derivatives of M

  tmp1 <- (uF - alF) / sigF
  tmp2 <- (lF - alF) / sigF

  #dnorm((uF-alF)/sigF)/dnorm((uF-alF)/sigF)
  upF <- 1

  #dnorm((lF-alF)/sigF)/dnorm((lF-alF)/sigF)
  lpF <- 1.0
  MpF <- tmp1 * dnorm(tmp1) * upF + S.G2.n(tmp1, 0.0) -
           (tmp2 * dnorm(tmp2) * lpF + S.G2.n(tmp2, 0.0))
  MpF <- MpF / (1.0 - 2.0 * beta)

  #Derivative of D(1-gam)=D2
  A <- (m1F + D2 / sigF)
  B <- (m1F - D2 / sigF)
  dnormA <- dnorm(A)
  dnormB <- dnorm(B)
  D2pF <- (1.0 - MpF) * (dnormA - dnormB)
  D2pF <- D2pF / (dnormA + dnormB)

  #Derivative of sF(1-gam)
  S2pF <- A * dnormA * (MpF + D2pF) + S.G2.n(A, 0.0) +
            B * dnormB * (MpF - D2pF) + S.G2.n(B, 0.0) -
              mF * (dnormA * (MpF + D2pF - 1.0) + dnormB * (MpF - D2pF - 1)) -
                MpF * (B1 + B2)

  #Derivative of D(gam)=D1
  A <- (m1F + D1 / sigF)
  B <- (m1F - D1 / sigF)
  D1pF <- (1.0 - MpF) * (dnormA - dnormB)
  D1pF <- D1pF / (dnormA + dnormB)
   
  #Derivative of sF(gam)
  S1pF <- A * dnormA * (MpF + D1pF) + S.G2.n(A, 0.0) +
            B * dnormB * (MpF - D1pF) + S.G2.n(B, 0.0) -
              m1F * (dnormA * (MpF + D1pF - 1.0) + dnormB * (MpF - D1pF - 1.0)) -
                MpF * (B3 + B4)

  #Derivative of S=S2-S1
  SpF <- (S2pF - S1pF) / (1.0 - 2.0 * gam)

  mu1F <- exp(0.5 * sigF^2)
  muF <- exp(alF + 0.5 * sigF^2)

  #S.quql.l(u,0,sigF)
  z <- S.quql.l(u, 0.0, sigF, 1e-4)

  qu1F <- z$qu
  ql1F <- z$ql

  #S.quql.l(u,alF,sigF)
  z <- S.quql.l(u, alF, sigF, 1e-4)

  quF <- z$qu
  qlF <- z$ql

  #S.K1.l(quF/exp(alF),sigF)/exp(alF)
  fquF <- S.K1.l(quF / exp(alF), sigF) / exp(alF)

  #S.K1.l(qlF/exp(alF),sigF)/exp(alF)
  fqlF <- S.K1.l(qlF / exp(alF), sigF) / exp(alF)

  #S.K.l(qlF/exp(alF),sigF)
  FqlF <- S.K.l(qlF / exp(alF), sigF)

  #S.H0.l(u,alF,sigF)
  H0 <- u
  xsqu <- exp(sigF * qnorm(u))
  expalF <- exp(alF)

  #S.H1.l(u,alF,sigF)
  H1 <- expalF * S.G.l(xsqu, sigF)

  #S.J0.l(u,alF,sigF)
  J0 <- S.K.l(qlF / expalF, sigF)

  #S.J1.l(u,alF,sigF)
  J1 <- expalF * S.G.l(qlF / expalF, sigF)

  #S.K.l(ql1F,sigF)
  Kl1 <- S.K.l(ql1F, sigF)

  #S.K1.l(ql1F,sigF)
  K1l1 <- S.K1.l(ql1F, sigF)

  #S.K1.l(qu1F,sigF)
  K1u1 <- S.K1.l(qu1F, sigF)

  #S.K2.l(qu1F,sigF)
  K2u1 <- S.K2.l(qu1F, sigF)

  #S.K2.l(ql1F,sigF)
  K2l1 <- S.K2.l(ql1F, sigF)

  #S.G1.l(qu1F,sigF)
  G1u1 <- qu1F * dlnorm(qu1F, 0.0, sigF)

  #S.G1.l(ql1F,sigF)
  G1l1 <- ql1F * dlnorm(ql1F, 0.0, sigF)

  #S.G2.l(qu1F,sigF)
  G2u1 <- S.G2.l(qu1F, sigF)

  #S.G2.l(ql1F,sigF)
  G2l1 <- S.G2.l(ql1F, sigF)

  Theta <- list(iopt = iopt, alF = alF, sigF = sigF, beta = beta, gam = gam,
                mF = mF, m1F = m1F, D1 = D1, D2 = D2, sF = sF, s1F = s1F,
                uF = uF, lF = lF, W2beta = W2beta, A1 = A1, A2 = A2, A3 = A3,
                A4 = A4, B1 = B1, B2 = B2, B3 = B3, B4 = B4, QGup1 = QGup1,
                QGlow1 = QGlow1, QGup2 = QGup2, QGlow2 = QGlow2, fm = fm,
                fQGup1 = fQGup1, fQGlow1 = fQGlow1, fQGup2 = fQGup2,
                fQGlow2 = fQGlow2, MpF = MpF, SpF = SpF, u = u, mu1F = mu1F,
                muF = muF, qu1F = qu1F, quF = quF, ql1F = ql1F, qlF = qlF,
                fquF = fquF, fqlF = fqlF, FqlF = FqlF, H0 = H0, H1 = H1,
                J0 = J0, J1 = J1, Kl1 = Kl1, K1l1 = K1l1, K1u1 = K1u1,
                K2u1 = K2u1, K2l1 = K2l1, G1u1 = G1u1, G1l1 = G1l1, G2u1 = G2u1,
                G2l1 = G2l1, D2pF = D2pF, D1pF = D1pF, S1pF = S1pF, S2pF = S2pF)

  Theta
}



#S.TD.fun.l(1, ...)
S.quql.l <- function(u, alpha, sigma, tol = 1e-4)
{
  z <- .Fortran("rlquqldl",
                u = as.double(u),
                alpha = as.double(alpha),
                sigma = as.double(sigma),
                tol = as.double(tol),
                ql = double(1),
                qu = double(1),
                isol = integer(1),
                PACKAGE = "robust")

  list(ql = z$ql, qu = z$qu, ok = z$isol)
}


#S.TD.fun.l(2, ...)
S.G2.n <- function(t, alpha = 0.0)
{
  z0 <- t - alpha
  -(z0 + alpha) * dnorm(z0) + pnorm(z0)
}


#S.TD.fun.l(3, ...)
S.G2.l <- function(t, sigma = 1.0)
{
  t[t <= 0] <- 1e-4
  z0 <- (log(t) - sigma^2) / sigma
  I1 <- -z0 * dnorm(z0) + pnorm(z0)

  #S.K.n(z0)
  I2 <- pnorm(z0)

  #S.G.n(z0)
  I3 <- -dnorm(z0)

  I <- sigma^2 * exp(sigma^2 / 2.0) * (I1 + sigma^2 * I2 + 2.0 * sigma * I3)
  u0 <- log(t) / sigma

  #S.G.l(t,sigma)
  tmp <- exp(0.5 * sigma^2) * pnorm(u0 - sigma)
  1.0 / sigma^3 * I - 1.0 / sigma * tmp
}


#S.TD.fun.l(4, ...)
S.G.n <- function(t, alpha = 0.0)
{
  z0 <- t - alpha
  -dnorm(z0) + alpha * pnorm(z0)
}


#S.TD.fun.l(5, ...)
S.G.l <- function(t, sigma)
{
  t[t <= 0] <- 1e-4 
  u0 <- log(t) / sigma
  exp(0.5 * sigma^2) * pnorm(u0 - sigma)
}


#S.TD.fun.l(6, ...)
S.K.l <- function(t, sigma)
{
  t[t <= 0] <- 1e-4 
  pnorm(log(t) / sigma)
}


#S.TD.fun.l(7, ...)
S.K1.l <- function(t, sigma)
{
  t[t <= 0] <- 1e-4
  dnorm(log(t) / sigma) / (t * sigma)
}


#S.TD.fun.l(8, ...)
S.K2.l <- function(t, sigma)
{
  t[t <= 0] <- 1e-4
  z0 <- log(t) /sigma
  1.0 / sigma * (-z0 * dnorm(z0))
}




################################################################################
##
## Auxilliary functions for robust estimation of weibull distribution
## parameters.
##
################################################################################

S.Theta.weibull <- function(shape, scale, u = 0.99, beta = 0.4, gam = 0.4)
{
  #Theta <- S.Theta.D.w(shape,scale,u)
  if(abs(beta - 0.5) <= 1e-4) {

    mu1F <- gamma(1 + 1 / shape)
    muF <- scale * gamma(1 + 1 / shape)

    #S.med.w(shape,scale)
    medF <- scale * qweibull(0.5, shape)

    tmp <- S.qad1.w(shape, 0.5, 0.5)$qad1

    #S.mad.w(shape,scale)
    madF <- scale * tmp

    #S.med.w(shape,1)
    M1F <- qweibull(0.5, shape)

    #S.mad.w(shape,1)
    D1F <- tmp

    #S.K1.w(medF/scale,shape)/scale
    fmed <- dweibull(medF / scale, shape) / scale

    #S.K1.w(M1F+D1F,shape)/scale
    fMpD <- dweibull(M1F + D1F, shape) / scale

    #S.K1.w(M1F-D1F,shape)/scale
    fMmD <- dweibull(M1F - D1F, shape) / scale

    MpF <- -S.K2.w(M1F, shape) / dgamma(M1F, shape)  
    A <- M1F + D1F
    B <- M1F - D1F
    DpF <- S.K2.w(B, shape) - S.K2.w(A, shape) -
             MpF * (dweibull(A, shape) - dweibull(B, shape))
    DpF <- DpF / (dweibull(A, shape) + dweibull(B, shape))     

    #S.quql.w(u,shape,1)
    z <- S.quql.w(u, shape, 1.0)

    qu1F <- z$qu
    ql1F <- z$ql

    #S.quql.w(u,shape,scale)
    z <- S.quql.w(u, shape, scale)

    quF <- z$qu
    qlF <- z$ql
    fquF <- dweibull(quF / scale, shape) / scale
    fqlF <- dweibull(qlF / scale, shape) / scale

    #S.K.w(ql1F,shape)
    FqlF <- pweibull(ql1F, shape)

    #S.H0.w(u,shape,scale)
    H0 <- u
    tmp <- qweibull(u, shape, 1.0)

    #S.H1.w(u,shape,scale)
    H1 <- scale * S.G.w(tmp, shape)

    #S.J0.w(u,shape,scale)
    J0 <- pweibull(ql1F, shape)

    #S.J1.w(u,shape,scale)
    J1 <- scale * S.G.w(ql1F, shape)

    #S.K.w(ql1F,shape)
    Kl1 <- pweibull(ql1F, shape)

    K1l1 <- dweibull(ql1F, shape)
    K1u1 <- dweibull(qu1F, shape)

    #S.K2.w(qu1F,shape)
    K2u1 <- S.K2.w(qu1F, shape)

    K2l1 <- S.K2.w(ql1F, shape)

    #S.G1.w(qu1F,shape)
    G1u1 <- qu1F * dweibull(qu1F, shape)

    #S.G1.w(ql1F,shape)
    G1l1 <- ql1F * dweibull(ql1F, shape)

    #S.G2.w(qu1F,shape)
    G2u1 <- S.G2.w(qu1F, shape)

    #S.G2.w(ql1F,shape)
    G2l1 <- S.G2.w(ql1F, shape)

    Theta <- list(iopt = 2, u = u, mu1F = mu1F, muF = muF, qu1F = qu1F,
                  quF = quF, ql1F = ql1F, qlF = qlF, fquF = fquF,
                  fqlF = fqlF, FqlF = FqlF, H0 = H0, H1 = H1, J0 = J0,
                  J1 = J1, Kl1 = Kl1, K1l1 = K1l1, K1u1 = K1u1, K2u1 = K2u1,
                  K2l1 = K2l1, G1u1 = G1u1, G1l1 = G1l1, G2u1 = G2u1,
                  G2l1 = G2l1, alF = shape, sigF = scale, M1F = M1F,
                  D1F = D1F, MpF = MpF, DpF = DpF, medF = medF, madF = madF,
                  fmed = fmed, fMpD = fMpD, fMmD = fMmD)
  }

  #Theta <- S.Theta.Dsm.w(shape,scale,u,beta,gam)
  else {

    tau <- log(scale)
    v <- 1/shape
    tol <- 1e-4

    mF <- .Fortran("rltrmnlw",
                   alpha = as.double(shape),
                   sigma = as.double(scale),
                   beta = as.double(beta),
                   mf = double(1),
                   PACKAGE = "robust")$mf

    #z <- S.trmadv.lw(1,beta,gam)
    z <- .Fortran("rltrmadlw",
                  alpha = as.double(1.0),
                  beta = as.double(beta),
                  gam = as.double(gam),
                  tol = as.double(tol),
                  mf = double(1),
                  sf = double(1),
                  isol = integer(1),
                  PACKAGE = "robust")

    m1F <- z$mf
    s1F <- z$sf

    #S.K1.lw(m1F)/v
    fm <- S.dlweib(m1F, 1.0, 1.0) / v

    #S.qad1.lw(beta,gam)$qad1/shape
    D2 <- .Fortran("rlqad1lw",
                   beta = as.double(beta),
                   gam = as.double(gam),
                   tol=  as.double(tol),
                   qad1 = double(1),
                   isol = integer(1),
                   PACKAGE = "robust")$qad1/shape

    #S.qad1.lw(beta,1-gam)$qad1/shape
    D1 <- .Fortran("rlqad1lw",
                   beta = as.double(beta),
                   gam = as.double(1-gam),
                   tol = as.double(tol),
                   qad1 = double(1),
                   isol = integer(1),
                   PACKAGE = "robust")$qad1/shape

    QGup1 <- D1+mF
    QGlow1 <- -D1+mF
    tmp <- (QGup1 - tau) / v
    fQGup1 <- S.dlweib(tmp, 1.0, 1.0) / v #S.K1.lw((QGup1-tau)/v)/v
    tmp <- (QGlow1 - tau) / v
    fQGlow1 <- S.dlweib(tmp, 1.0, 1.0) / v #S.K1.lw((QGlow1-tau)/v)/v
    QGup2 <- D2 + mF
    QGlow2 <- -D2 + mF
    tmp <- (QGup2 - tau) / v
    fQGup2 <- S.dlweib(tmp, 1.0, 1.0) / v #S.K1.lw((QGup2-tau)/v)/v
    tmp <- (QGlow2 - tau) / v
    fQGlow2 <- S.dlweib(tmp, 1.0, 1.0) / v #S.K1.lw((QGlow2-tau)/v)/v  

    #S.K.lw((QGup2-tau)/v)
    B1 <- S.K.lw((QGup2 - tau) / v)

    #S.K.lw((QGlow2-tau)/v)
    B2 <- S.K.lw((QGlow2 - tau) / v)

    #S.K.lw((QGup1-tau)/v)
    B3 <- S.K.lw((QGup1 - tau) / v)

    #S.K.lw((QGlow1-tau)/v)
    B4 <- S.K.lw((QGlow1 - tau) / v)

    #S.G.lw((QGup2-tau)/v)+tau*B1/v)*v
    A1  <-  (S.G.lw((QGup2 - tau) / v) + tau * B1 / v) * v
    A2 <- (S.G.lw((QGlow2 - tau) / v) + tau * B2 / v) * v
    A3 <- (S.G.lw((QGup1 - tau) / v) + tau * B3 / v) * v
    A4 <- (S.G.lw((QGlow1 - tau) / v) + tau * B4 / v ) * v

    #S.trmadv.lw(shape,beta,gam)$sF
    sF <- .Fortran("rltrmadlw",
                   alpha = as.double(shape),
                   beta = as.double(beta),
                   gam = as.double(gam),
                   tol = as.double(tol),
                   mf = double(1),
                   sf = double(1),
                   isol = integer(1),
                   PACKAGE = "robust")$sf

    uF <- log(qweibull(1.0 - beta, 1.0)) * v + tau
    lF <- log(qweibull(beta, 1.0)) * v + tau
    W2beta <- (1.0 - 2.0 * beta) * mF + beta * lF + beta * uF

    mu1F <- gamma(1.0 + 1.0 / shape)
    muF <- scale * gamma(1.0 + 1.0 / shape) 

    #S.quql.w(u,shape,1)
    z <- S.quql.w(u, shape, 1.0)
    qu1F <- z$qu
    ql1F <- z$ql

    #S.quql.w(u,shape,scale)
    z <- S.quql.w(u, shape, scale)
    quF <- z$qu
    qlF <- z$ql

    #S.K1.w(qu1F,shape)/scale
    fquF <- dweibull(qu1F, shape) / scale

    #S.K1.w(ql1F,shape)/scale
    fqlF <- dweibull(ql1F, shape) / scale

    #S.K.w(ql1F,shape)
    FqlF  <- pweibull(ql1F, shape)

    #S.H0.w(u, shape, scale)
    H0 <- u
    tmp <- qweibull(u, shape, 1.0)

    #S.H1.w(u,shape,scale)
    H1 <- scale * S.G.w(tmp, shape)

    #S.J0.w(u,shape,scale)
    J0 <- pweibull(ql1F,shape)

    #S.J1.w(u,shape,scale)
    J1 <- scale * S.G.w(ql1F, shape)

    #S.K.w(ql1F,shape)
    Kl1 <- pweibull(ql1F, shape)

    #S.K1.w(ql1F,shape)
    K1l1 <- fqlF * scale

    #S.K1.w(qu1F,shape)
    K1u1 <- fquF * scale

    #S.K2.w(qu1F,shape)
    K2u1 <- S.K2.w(qu1F, shape)

    #S.K2.w(ql1F,shape)
    K2l1 <- S.K2.w(ql1F, shape)

    #S.G1.w(qu1F,shape)
    G1u1 <- qu1F * dweibull(qu1F, shape)

    #S.G1.w(ql1F,shape)
    G1l1 <- ql1F * dweibull(ql1F, shape)

    #S.G2.w(qu1F,shape)
    G2u1 <- S.G2.w(qu1F, shape)

    #S.G2.w(ql1F,shape)
    G2l1 <- S.G2.w(ql1F, shape)

    MpF <- 0
    SpF <- 0

    Theta <- list(iopt = 1, alF = shape, sigF = scale, beta = beta,
                  gam = gam, mF = mF, m1F = m1F, D1 = D1, D2 = D2,
                  sF = sF, s1F = s1F, uF = uF, lF = lF, W2beta = W2beta,
                  A1 = A1, A2 = A2, A3 = A3, A4 = A4, B1 = B1, B2 = B2,
                  B3 = B3, B4 = B4, QGup1 = QGup1, QGlow1 = QGlow1,
                  QGup2 = QGup2, QGlow2 = QGlow2, fm = fm, fQGup1 = fQGup1,
                  fQGlow1 = fQGlow1, fQGup2 = fQGup2, fQGlow2 = fQGlow2,
                  MpF = MpF, SpF = SpF, u = u, mu1F = mu1F, muF = muF,
                  qu1F = qu1F, quF = quF, ql1F = ql1F, qlF = qlF,
                  fquF = fquF, fqlF = fqlF, FqlF = FqlF, H0 = H0, H1 = H1,
                  J0 = J0, J1 = J1, Kl1 = Kl1, K1l1 = K1l1, K1u1 = K1u1,
                  K2u1 = K2u1, K2l1 = K2l1, G1u1 = G1u1, G1l1 = G1l1,
                  G2u1 = G2u1, G2l1 = G2l1)
  }

  Theta
}


#S.TD.fun.w(1, ...)
S.quql.w <- function(u, alpha, sigma, tol = 1e-4)
{
  z <- .Fortran("rlquqldw",
                u = as.double(u),
                alpha = as.double(alpha),
                sigma = as.double(sigma),
                tol = as.double(tol),
                ql = double(1),
                qu = double(1),
                isol = integer(1),
                PACKAGE = "robust")

  list(ql = z$ql, qu = z$qu, ok = z$isol)
}


#S.TD.fun.w(2, ...)
S.qad1.w <- function(alpha, beta, gam, tol = 1e-4)
{
  z <- .Fortran("rlqad1w",
                alpha = as.double(alpha),
                beta = as.double(beta),
                gam = as.double(gam),
                tol = as.double(tol),
                qad1 = double(1),
                isol = integer(1),
                PACKAGE = "robust")

  list( qad1 = z$qad1, ok = z$isol)
}


#S.TD.fun.w(3, ...)
S.dlweib <- function(y, sigma = 1.0, alpha = 1.0)
{
  tau <- log(sigma)
  v <- 1.0 / alpha
  t <- (y - tau) / v

  (1 / v) * exp(t - exp(t))
}


#S.TD.fun.w(4, ...)
S.K.lw <- function(t, sigma = 1.0, alpha = 1.0)
{
  y <- exp(t)
  pweibull(y, alpha, sigma)
}


#S.TD.fun.w(5, ...)
S.G.w <- function(t, alpha)
{
  a1 <- 1.0 + 1.0 / alpha
  a2 <- 2.0 + 1.0 / alpha
  ga1 <- gamma(a1)
  p <- pweibull(t, alpha)
  mup <- p * ga1
  lp <- log(1.0 / (1.0 - p))

  for(k in 1:length(p)) {
    if(p[k] == 1)
      break

    pa <- 1.0 / alpha[k] + 1.0
    ig <- .Fortran("rlingama",
                   x = as.double(lp[k]),
                   p = as.double(pa),
                   g = double(1),
                   PACKAGE = "robust")$g

    mup[k] <- ga1[k] * ig
  }

  mup
}


#S.TD.fun.w(6, ...)
S.K2.w <- function(t, alpha)
{
  z0 <- t^alpha
  one <- 1
  two <- 2

  #S.Intlgam(t, alpha)
  tmp1 <- .Fortran("rlsumlgm",
                   hi = as.double(z0),
                   alpha = as.double(one),
                   gl = double(1),
                   PACKAGE = "robust")$gl

  #S.Intlgam(t, alpha)
  tmp2 <- .Fortran("rlsumlgm",
                   hi = as.double(z0),
                   alpha = as.double(two),
                   gl = double(1),
                   PACKAGE = "robust")$gl

  (1.0 / alpha) * (pweibull(t, alpha) + tmp1 - gamma(2) * tmp2)
}


#S.TD.fun.w(7, ...)
S.G2.w <- function(t, alpha)
{
  z0 <- t^alpha
  a1 <- 1.0 + 1.0 / alpha
  a2 <- 2.0 + 1.0 / alpha
  ga1 <- gamma(a1)
  p <- pweibull(t, alpha)
  mup <- p * ga1
  lp <- log(1.0 / (1.0 - p))

  for(k in 1:length(p)){
    if(p[k]==1)
      break

    pa <- 1.0 / alpha[k] + 1.0

    ig <- .Fortran("rlingama",
                    x = as.double(lp[k]),
                    p = as.double(pa),
                    g = double(1),
                    PACKAGE = "robust")$g

    mup[k] <- ga1[k] * ig
  }

  tmp1 <- .Fortran("rlsumlgm",
                    hi = as.double(z0),
                    alpha = as.double(a1),
                    gl = double(1),
                    PACKAGE = "robust")$gl

  tmp2 <- .Fortran("rlsumlgm",
                    hi = as.double(z0),
                    alpha = as.double(a2),
                    gl = double(1),
                    PACKAGE = "robust")$gl

  (1.0 / alpha) * (mup + ga1 * tmp1 - gamma(a2) * tmp2)
}


#S.TD.fun.w(8, ...)
S.G.lw <- function(t, sigma = 1.0)
{
  lsg <- log(sigma)
  et <- exp(t - lsg)

  #S.Intlgam(et, 1)
  z <- .Fortran("rlsumlgm",
                hi = as.double(et),
                alpha = as.double(1.0),
                gl = double(1),
                PACKAGE = "robust")$gl

  z + lsg * (1.0 - exp(-et))
}


Tab.weibull <- function(b1 = 1.5, b2 = 1.7, A = c(0, 0, 0), monit = 0,
                        maxta = 1, maxtc = 1, maxit = 30, til = 0.001,
                        tol = 0.001)
{
  if(abs(b1 - b2) < 1e-5 && b1 < 1.075)
    warning("Solution for b1 = b2 & b1 < 1.075 might not exist!")

  if(monit != 0) {
    cat("Alfa,  Nit,  f(c1),  f(c2), fa(1),  fa(2), fa(3) \n")
    cat("nsol,  x2(1),  x2(2),  x2(3),  x2(4):\n")
  }

  f.res <- .Fortran("rlcretabw",
                    b1 = as.double(b1),
                    b2 = as.double(b2),
                    a = as.double(A),
                    maxta = as.integer(maxta),
                    maxtc = as.integer(maxtc),
                    maxit = as.integer(maxit),
                    til = as.double(til),
                    tol = as.double(tol),
                    monit = as.integer(monit),
                    tab = double(5),
                    tpar = double(6),
                    PACKAGE = "robust")

  f.res$tab <- array(f.res$tab, c(NULL,5))
  dimnames(f.res$tab) <- list(c("c1*v", "c2*v", "a11/v", "a21/v", "a22/v"))
  list(Table = f.res$tab)
}


