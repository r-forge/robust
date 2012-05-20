panel.addons <- function(x, y, smooths = FALSE, rugplot = FALSE, id.n = 3)
{
  if(smooths)
    panel.loess(x, y, col = "black")
  if(rugplot)
    panel.rug(x, col = "black")
  if(id.n > 0) {
    n <- length(y)
    out <- order(abs(y))[(n - id.n + 1):n]
    panel.text(x[out], y[out], paste(" ", out, sep = ""), adj = 0)
  }
  invisible()
}


