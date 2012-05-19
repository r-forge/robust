print.aovRob <- function(x, ...)
{
  ss <- as.data.frame(anova.lmRob(x, test="RF"))
  tmp.d <- dim(ss)
  tmp.n <- dimnames(ss)
  ss <- ss[-1,-tmp.d[2]]
  if (!is.null(cl <- x$call)) {
    cat("Call:\n")
    dput(cl)
  }
  cat("\nTerms:\n")
  prmatrix(t(array(	c(format(zapsmall(ss[,2])), format(ss[,1])), 
										c(tmp.d[1]-1, 2), list(tmp.n[[1]][-1],
										rev(tmp.n[[2]][-tmp.d[2]])))), quote = F,
										right = T)
  cat("\nRobust residual scale:", format(x$scale), "\n")
  invisible(x)
}

