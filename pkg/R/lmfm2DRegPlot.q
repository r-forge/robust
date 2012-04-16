lmfm2DRegPlot <- function(x, cutoff = TRUE, main, xlab, ylab, ...) 
{
  n.models <- length(x)
  mod.names <- names(x)

  model <- sapply(x, function(u) !is.null(u$model))

  if(!any(model))
    stop("none of the fitted models in ", sQuote(deparse(substitute(x))),
         "contain a model frame component")

  model <- x[[(1:n.models)[model][1]]]$model

  if(ncol(model) != 2)
    stop("This method requires a simple linear regression model")

  var.names <- attributes(model)$names

  if(missing(main))
    main <- paste(var.names, collapse = " ~ ")

  if(missing(xlab))
    xlab <- var.names[2]

  if(missing(ylab))
    ylab <- var.names[1]

  plot(model[[2]], model[[1]],
       type = "p",
       xlab = xlab,
       ylab = ylab,
       main = main,
       pch = 16,
       col = 6)

  the.lines <- integer(n.models)
  the.wd <- integer(n.models)

  for(i in 1:n.models) {
    if(length(grep("Rob", x[[i]]$call))) {
      a <- x[[i]]$yc * x[[i]]$scale
      if(cutoff) abline(coef(x[[i]]) + c(-a, 0), lty = 2)
      abline(coef(x[[i]]), lwd = 2)
      if(cutoff) abline(coef(x[[i]]) + c(a, 0), lty = 2)
      the.lines[i] <- 1
      the.wd[i] <- 2
    }

    else {
      abline(coef(x[[i]]), lty = 4)
      the.lines[i] <- 4
      the.wd[i] <- 1
    }
  }

  pos <- ifelse(coef(x[[1]])[2] > 0, "topleft", "topright")
  legend(x = pos, legend = mod.names, lty = the.lines, lwd = the.wd, bty = "n")

  invisible(x)
}


