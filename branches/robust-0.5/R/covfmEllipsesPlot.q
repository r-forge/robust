covfmEllipsesPlot <- function(x, main, xlab, ylab, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  if(missing(main))
    main <- "default"

  if(missing(xlab))
    xlab <- "default"

  if(missing(ylab))
    ylab <- "default"

  p <- dim(x[[1]]$cov)[1]

  old.par <- par(pty = "s")
  on.exit(par(old.par))

  ## if p == 2 plot data with overlaid ellipse ##
  if(p == 2) {

    ellipse <- function(loc, A)
    {
      detA <- A[1, 1] * A[2, 2] - A[1, 2]^2
      dist <- sqrt(qchisq(0.95, 2))
      ylimit <- sqrt(A[2, 2]) * dist
      y <- seq(-ylimit, ylimit, 0.01 * ylimit)
      sqrt.discr <- detA/A[2,2]^2 * (A[2,2] * dist^2 - y^2)
      sqrt.discr[c(1, length(sqrt.discr))] <- 0.0
      sqrt.discr <- sqrt(sqrt.discr)
      b <- loc[1] + A[1, 2] / A[2, 2] * y
      x1 <- b - sqrt.discr
      x2 <- b + sqrt.discr
      y <- loc[2] + y
      rbind(cbind(x1, y), cbind(rev(x2), rev(y)))
    }

    z <- list()
    x.min <- Inf
    x.max <- -Inf
    y.min <- Inf
    y.max <- -Inf

    for(i in 1:n.models) {
      z[[i]] <- ellipse(x[[i]]$center, x[[i]]$cov)
      x.min <- min(x.min, z[[i]][,1])
      x.max <- max(x.max, z[[i]][,1])
      y.min <- min(y.min, z[[i]][,2])
      y.max <- max(y.max, z[[i]][,2])
    }

    points <- eval(x[[1]]$call$data)
    x.min <- min(x.min, points[,1])
    x.max <- max(x.max, points[,1])
    y.min <- min(y.min, points[,2])
    y.max <- max(y.max, points[,2])
    center <- c(mean(c(x.min, x.max)), mean(c(y.min, y.max)))

    s.range <- max(abs(c(center[1] - x.min, x.max - center[1],
                         center[2] - y.min, y.max - center[2])))

    if(n.models == 1)
      header <- "95% Tolerance Ellipse"
    else
      header <- "95% Tolerance Ellipses"

    if(main == "default")
      main <- header

    if(xlab == "default")
      xlab <- dimnames(points)[[2]][1]

    if(ylab == "default")
      ylab <- dimnames(points)[[2]][2]

    plot(points[,1], points[,2],
      xlim = c(center[1] - s.range, center[1] + s.range),
      ylim = c(center[2] - s.range, center[2] + s.range),
      pch = 16,
      main = main,
      xlab = xlab,
      ylab = ylab)

    for(i in 1:length(z))
      polygon(z[[i]], density = 0, lty = i, col = i, lwd = i)

    pos <- ifelse(x[[1]]$cov[1,2] > 0, "topleft", "topright")
    legend(x = pos, legend = mod.names, col = 1:n.models, lty = 1:n.models,
           lwd = 1:n.models, bty = "n")
  }

  ## if p > 2 plot matrix of ellipses ##
  else {
    if(main == "default")
      main <- ""

    if(xlab == "default")
      xlab <- ""

    if(ylab == "default")
      ylab <- ""

    plot(0, 0, xlim = c(0, p + 1), ylim = c(0, p + 1), type = "n",
         axes = FALSE, main = main, xlab = xlab, ylab = ylab)

    for(k in 1:n.models) {
      if(x[[k]]$corr)
        X <- x[[k]]$cov
      else {
        s <- sqrt(diag(x[[k]]$cov))
        X <- x[[k]]$cov / (s %o% s)
      }

      x.centers <- matrix(rep(1:p, p), byrow = TRUE, ncol = p)
      y.centers <- matrix(rep(p:1, p), ncol = p)
      points <- rep((c(0:180, NA) * pi)/90, (p^2 - p) / 2)
      cors <- as.vector(rbind(matrix(X[row(X) < col(X)],
                nrow = 181, ncol = (p^2 - p)/2, byrow = TRUE),
                rep(NA, (p^2 - p)/2)))
      xs <- 0.475 * cos(points + acos(cors)/2) +
                rep(x.centers[row(x.centers) < col(x.centers)], each = 182)
      ys <- 0.475 * cos(points - acos(cors)/2) +
                rep(y.centers[row(x.centers) < col(x.centers)], each = 182)
      polygon(x = xs, y = ys, density = 0, col = k)
      shift <- max(0.2, (p - 8)/88 + 0.2)
      xs <- x.centers[row(x.centers) > col(x.centers)]
      ys <- y.centers[row(y.centers) > col(y.centers)]
      cors <- X[row(X) > col(X)]
      text(xs, ys + (((shift*(n.models - 1))/2) - shift*(k - 1)),
           labels = round(cors, digits = max(1, floor(20/p))),
           col = k, cex = min(1, 90/(p^2)))
    }

    lines(c(1, p), c(p, 1), lwd = 2)

    lcx <- cbind(1:p, rep(p + 0.7, p))
    lcy <- cbind(rep(0.5, p), p:1)
    text(lcx, labels = dimnames(X)[[2]], cex = 1, adj = 0, srt = 90)
    text(lcy, labels = dimnames(X)[[1]], cex = 1, adj = 1)

    legend(x = "bottom", legend = mod.names, lty = 1, col = 1:n.models,
           bty = "n")
  }

  invisible(x)
}


