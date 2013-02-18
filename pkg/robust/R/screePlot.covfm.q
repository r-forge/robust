screePlot.covfm <- function(x, npcs, strip = "", ...)
{
  n.models <- length(x)
  mod.names <- names(x)
  dots <- list(...)

  eval.extractor <- function(u)
    eigen(u$cov, symmetric = TRUE, only.values = TRUE)$values
  evals <- lapply(x, eval.extractor)
  
  if(missing(npcs))
    npcs <- min(10, max(sapply(evals, length)))

  for(i in 1:n.models)
    if(length(evals[[i]]) > npcs)
      evals[[i]] <- evals[[i]][1:npcs]

  n.evals <- sapply(evals, length)

  tdf <- data.frame(evals = unlist(evals),
                    index = unlist(lapply(n.evals, function(u) 1:u)),
                    mod = factor(rep(mod.names, n.evals), levels = mod.names))

  key <- simpleKey(text = mod.names, lines = TRUE, x = 0.95, y = 0.925, corner = c(1, 1), ...)

  if(!is.null(pch <- dots$pch) && length(pch) == n.models)
    key$points$pch <- pch

  x.scale <- list(x = list(at = 1:npcs,
                           labels = paste("Comp", 1:npcs, sep = ".")))

  p <- xyplot(evals ~ index | strip,
              data = tdf,
              groups = mod,
              strip = function(...) strip.default(..., style = 1),
              type = "b",
              key = key,
              scales = x.scale,
              ...)

  print(p)
  invisible(p)
}


