print.lmfm <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
  n.models <- length(x)
  fancy.names <- format(paste(names(x), ":", sep = ""), justify = "right")

  cat("\nCalls:\n")
  for(i in 1:n.models) {
    cat(fancy.names[i], " ", sep = "")
    print(x[[i]]$call, ...)
  }

  cat("\nCoefficients:\n")
  printCoefmat(t(coef(x)), digits = digits, P.values = FALSE, na.print = "",
               signif.stars = FALSE, ...)
  cat("\n")

  invisible(x)
}


