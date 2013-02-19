print.fdfm <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
	n.models <- length(x)
	mod.names <- names(x)

  p.names <- unique(as.vector(sapply(x, function(u) names(u$estimate))))
  n.params <- length(p.names)
  estimates <- matrix(NA, n.params, 2*n.models)
  dimnames(estimates) <- list(p.names, NULL)

  for(i in 1:n.models) {
    estimate <- x[[i]]$estimate
    estimates[names(estimate), 2*i - 1] <- estimate
    estimates[names(estimate), 2*i] <- x[[i]]$sd
  }
  estimates <- format(estimates, digits = digits)#, ...)
  strlens <- nchar(estimates)
  NAs <- substr(estimates, strlens - 1, strlens) == "NA"
  substr(estimates[NAs], strlens - 1, strlens) <- "  "

  for(j in 1:n.models) {
    for(i in 1:length(p.names)) {
      el <- estimates[i, 2*j]
      if(substr(el, 1, 1) == " ")
        estimates[i, 2*j] <- paste(" ", el, "   ", sep = "")
      else
        estimates[i, 2*j] <- paste("(", el, ")  ", sep = "")
    }
  }

  for(i in 1:n.params)
    for(j in 1:n.models)
      estimates[i, j] <- paste(estimates[i, (2*j - 1):(2*j)], collapse = " ")
  estimates <- estimates[, 1:n.models, drop = FALSE]
  dimnames(estimates) <- list(p.names, mod.names)

  print(estimates, quote = FALSE)
	invisible(x)
}


