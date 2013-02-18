print.fitdstn <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
  ans <- format(rbind(x$estimate, x$sd), digits = digits)
  ans[1, ] <- sapply(ans[1, ], function(x) paste("", x))
  ans[2, ] <- sapply(ans[2, ], function(x) paste("(", x, ")", sep = ""))
  dn <- dimnames(ans)
  dn[[1]] <- rep("", 2)
  dn[[2]] <- paste(substring("      ", 1, (nchar(ans[2, ]) - nchar(dn[[2]])) %/% 2), dn[[2]])
  dn[[2]] <- paste(dn[[2]], substring("      ", 1, (nchar(ans[2, ]) - nchar(dn[[2]])) %/% 2))
  dimnames(ans) <- dn

  print(ans, quote = FALSE)

  invisible(x)
}


