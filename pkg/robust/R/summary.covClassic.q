summary.covClassic <- function(object, ...)
{
  evals <- eigen(object$cov, symmetric = TRUE, only.values = TRUE)$values
  names(evals) <- paste("Eval.", 1:length(evals))
  object$evals <- evals

  object <- object[c("call", "cov", "center", "evals", "dist", "corr")]
  oldClass(object) <- "summary.covClassic"
  object
}


