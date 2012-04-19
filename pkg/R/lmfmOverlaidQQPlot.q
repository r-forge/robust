lmfmOverlaidQQPlot <- function(x, pch, col, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  if(missing(pch))
    pch <- 1:n.models

  if(missing(col))
    col <- 1:n.models

  settings <- list(superpose.symbol = list(pch = pch, col = col))

  res <- as.matrix(sapply(x, residuals))
  mod <- factor(rep(mod.names, each = nrow(res)), levels = mod.names)

  tdf <- data.frame(res = as.vector(res),
                    mod = mod)

  p <- qqmath(~ res | "",
              groups = mod,
              data = tdf,
              distribution = qnorm,
              strip = function(...) strip.default(..., style = 1),
              auto.key = list(corner = c(0.05, 0.95)),
              par.settings = settings,
              ...)

  print(p)
  invisible(p)
}


