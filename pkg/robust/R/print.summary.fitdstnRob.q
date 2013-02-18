print.summary.fitdstnRob <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
	cat("Call: ")
	print(x$call)
	cat("\nParameter Estimates (Std. Error):\n")
	print.fitdstn(x, digits = digits, ...)

  tab <- format(c(x$mu, sqrt(x$V.mu)), digits = digits, ...)
  tab[1] <- paste("", tab[1], "")
  tab[2] <- paste("(", tab[2], ")", sep = "")
  tab <- matrix(tab, 2, 1)
  dn <- list(rep("", 2), "mean")
  dn[[2]] <- paste(substring("      ", 1, (nchar(tab[2, ]) - 4) %/% 2), "mean")
  dn[[2]] <- paste(dn[[2]], substring("      ", 1, (nchar(tab[2, ]) - nchar(dn[[2]])) %/% 2))
  dimnames(tab) <- dn

  cat("\nEstimated Mean (Std. Error):\n")
  print(tab, quote = FALSE)

	invisible(x)
}


