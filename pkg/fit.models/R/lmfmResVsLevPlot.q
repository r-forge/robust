lmfmResVsLevPlot <- function(x, type = "response", id.n = 3, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  res <- lapply(x, resid, type = type)
  n.res <- sapply(res, length)

  leverages <- list()
  for(i in 1:n.models) {
    lev <- try(hatvalues(x[[i]]), silent = TRUE)

    if(class(lev) == "try-error")
      lev <- rep(NA_real_, n.res[i])

    leverages[[i]] <- lev
  }

  mod <- factor(rep(mod.names, n.res), levels = mod.names)
  tdf <- data.frame(res = unlist(res), leverages = unlist(leverages), mod = mod)

  panel.special <- function(x, y, id.n, ...) {
    panel.xyplot(x, y, ...)
    fit.models:::panel.addons(x, y, id.n = id.n)
    invisible()
  }

  p <- xyplot(res ~ leverages | mod,
              data = tdf,
              panel = panel.special,
              id.n = id.n,
              strip = function(...) strip.default(..., style = 1),
              layout = c(n.models, 1, 1),
              ...)

  print(p)
  invisible(p)
}


