stop.on.bdObject <- function(x)
{
	invisible()
}


key <- function(x, y, text, lines, corner = c(0, 1), cex = par("cex"),
	col = par("col"), lty = par("lty"), lwd = par("lwd"), pch = par("pch"),
  ...)
{
	if(missing(x))
		x <- "topright"

  if(!is.null(lines$col))
    col = lines$col

  if(!is.null(lines$lty))
    lty = lines$lty

  if(!is.null(lines$lwd))
    lwd = lines$lwd

	legend(x = x, y = y, legend = unlist(text), xjust = corner[1], yjust = corner[2],
		col = col, lty = lty, lwd = lwd, bty = "n")

	invisible()
}


splus.assign <- function(asgn, term.labels)
{
  if(min(asgn) == 0) {
    term.labels <- c("(Intercept)", term.labels)
    asgn <- asgn + 1
  }
  ans <- list()
  n <- length(term.labels)
  for(i in 1:n)
    ans[[i]] <- which(asgn == i)
  names(ans) <- term.labels
  ans
}


