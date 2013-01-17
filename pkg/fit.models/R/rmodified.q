rmodified <- function(object, ...)
  UseMethod("rmodified")


rmodified.default <- function(object, ...)
{
  qrx <- object$qr

  if(is.null(qrx))
    hii <- 0
  else
    hii <- apply(qr.Q(qrx)^2, 1, sum)

  residuals(object, ...) / sqrt(1.0 - hii)
}


