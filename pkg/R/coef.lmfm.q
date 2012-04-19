coef.lmfm <- function(object, ...)
{
  n.models <- length(object)
  coefs <- lapply(object, coef)
  coef.names <- list(names(coefs), names(coefs[[1]]))
  coefs <- matrix(unlist(coefs), nrow = n.models, byrow = TRUE)
  dimnames(coefs) <- coef.names
  coefs
}


