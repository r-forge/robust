print.lmfm <- function(x, ...)
{
  n.models <- length(x)
  fancy.names <- format(paste(names(x), ":", sep = ""), justify = "right")

  cat("\nCalls: \n")
  for(i in 1:n.models) {
    cat(fancy.names[i], " ", sep = "")
    print(x[[i]]$call)
  }

  cat("\nCoefficients:\n")
  print(t(coef(x)))
  cat("\n")

  sigmas <- devs <- rep(as.double(NA), n.models)

  for(i in 1:n.models) {
    if(is.null(x[[i]]$scale))
      sigmas[i] <- sqrt(sum(resid(x[[i]])^2) / x[[i]]$df.resid)
    else
      sigmas[i] <- x[[i]]$scale

    if(!is.null(x[[i]]$deviance))
      devs[i] <- x[[i]]$deviance
  }

  if(any(!is.na(sigmas))) {
    cat("Residual standard errors:\n")
    for(i in 1:n.models)
      cat(fancy.names[i], format(signif(sigmas[i], options()$digits)),
        "on", x[[i]]$df.resid, "degrees of freedom\n")
  }

  if(any(!is.na(devs))) {
    cat("Residual Deviance Estimates:\n")
    for(i in 1:n.models)
      cat(fancy.names[i], format(signif(devs[i], options()$digits)),
        "on", x[[i]]$df.resid, "degrees of freedom\n")
  }

  invisible(x)
}


