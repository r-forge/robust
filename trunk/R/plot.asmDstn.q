plot.asmDstn <- function(x, which.plots = ifelse(interactive(), "ask", "all"), ...)
{
  fm <- fit.models(x)
  names(fm) <- deparse(substitute(x))

  plot(fm, which.plots = which.plots, ...)

  invisible(x)
}


