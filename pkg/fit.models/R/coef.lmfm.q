coef.lmfm <- function(object, ...)
{
  n.models <- length(object)
  mod.names <- names(object)
  coefs <- lapply(object, coef)
  coef.names <- unique(unlist(lapply(coefs, names)))

  coefmat <- matrix(NA, n.models, length(coef.names))
  dimnames(coefmat) <- list(mod.names, coef.names)
  for(i in 1:n.models)
    coefmat[i, names(coefs[[i]])] <- coefs[[i]]

  coefmat
}


