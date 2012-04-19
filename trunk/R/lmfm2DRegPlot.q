lmfm2DRegPlot <- function(x, lwd, col, ...) 
{
  n.models <- length(x)
  mod.names <- names(x)

  mf <- sapply(x, function(u) !is.null(u$model))

  if(!any(mf))
    stop("none of the fitted models in ", sQuote(deparse(substitute(x))),
         "contain a model frame component")

  mf <- x[[(1:n.models)[mf][1]]]$model

  if(ncol(mf) != 2)
    stop(sQuote("x"), " is not a simple linear regression model")

  var.names <- attributes(mf)$names
  frm <- as.formula(paste(paste(var.names, collapse = " ~ "), " | \"\""))

  if(missing(lwd))
    lwd <- 1:n.models

  if(missing(col))
    col <- 1:n.models

  panel.special <- function(x, y, object, pch, lwd, col)
  {
    panel.xyplot(x, y, pch = 16, col = "black")

    for(i in 1:length(object)) {
      if(length(grep("Rob", object[[i]]$call))) {
        a <- object[[i]]$yc * object[[i]]$scale
        panel.abline(coef(object[[i]]) + c(-a, 0), lty = 2, col = col[i])
        panel.abline(coef(object[[i]]), lwd = lwd[i], col = col[i])
        panel.abline(coef(object[[i]]) + c(a, 0), lty = 2, col = col[i])
      }
      else {
        panel.abline(coef(object[[i]]), lwd = lwd[i], col = col[i])
      }
    }
    invisible()
  }

  if(coef(x[[1]])[2] > 0)
    corner <- c(0.05, 0.95)
  else
    corner <- c(0.95, 0.95)

  key <- simpleKey(corner = corner,
                   text = mod.names,
                   points = FALSE,
                   lines = TRUE)
  key$lines$col <- col
  key$lines$lwd <- lwd

  p <- xyplot(mf[[1]] ~ mf[[2]],
              panel = panel.special,
              object = x,
              col = col,
              lwd = lwd,
              xlab = var.names[2],
              ylab = var.names[1],
              key = key,
              ...)

  print(p)
  invisible(p)
}


