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
    MpF <- -S.K2n.g(M1F, alF) / dgamma(M1F, alF)
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




