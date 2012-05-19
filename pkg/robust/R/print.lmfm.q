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

  ## print method for lm shows only calls and coefs ##

#  sigmas <- devs <- rep(as.double(NA), n.models)
#
#  for(i in 1:n.models) {
#    if(is.null(x[[i]]$scale))
#      sigmas[i] <- sqrt(sum(resid(x[[i]])^2) / x[[i]]$df.resid)
#    else
#      sigmas[i] <- x[[i]]$scale
#
#    if(!is.null(x[[i]]$deviance))
#      devs[i] <- x[[i]]$deviance
#  }
#
#  if(any(!is.na(sigmas))) {
#    cat("Residual standard errors:\n")
#    for(i in 1:n.models)
#      cat(fancy.names[i], format(signif(sigmas[i], digits = digits)),
#        "on", x[[i]]$df.resid, "degrees of freedom\n")
#  }
#
#  if(any(!is.na(devs))) {
#    cat("Residual Deviance Estimates:\n")
#    for(i in 1:n.models)
#     cat(fancy.names[i], format(signif(devs[i], digits = digits)),
#        "on", x[[i]]$df.resid, "degrees of freedom\n")
#  }

  invisible(x)
}


