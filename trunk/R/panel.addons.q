panel.addons <- function(x, y, smooths = FALSE, rugplot = FALSE, id.n = 3, ...)
{
	if(smooths)
		panel.loess(x, y, col = 1, ...)
	if(rugplot)
		panel.rug(x, ...)
	if(id.n > 0) {
		n <- length(y)
		outliers <- order(abs(y))[(n - id.n + 1):n]
		panel.text(x[outliers], y[outliers], paste(" ", outliers, sep = ""), adj = 0)
	}
	invisible()
}


