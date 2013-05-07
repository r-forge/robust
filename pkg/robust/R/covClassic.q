covClassic <- function(data, corr = FALSE, center = TRUE, distance = TRUE,
                       na.action = na.fail, unbiased = TRUE, ...)
{
  the.call <- match.call(expand.dots = FALSE)

  data <- na.action(data)
  if(!is.matrix(data))
    data <- as.matrix(data)

  n <- nrow(data)
  p <- ncol(data)
  dn <- dimnames(data)
  dimnames(data) <- NULL
  rowNames <- dn[[1]]
  if(is.null(rowNames)) rowNames <- 1:n
  colNames <- dn[[2]]
  if(is.null(colNames)) colNames <- paste("V", 1:p, sep = "")

  if(length(center) != p && is.logical(center))
    center <- if(center) apply(data, 2, mean) else numeric(p)

  data <- sweep(data, 2, center)

  covmat <- crossprod(data) / (if(unbiased) (n - 1) else n)

  if(distance)
    dist <- mahalanobis(data, rep(0, p), covmat)

  if(corr) {
    std <- sqrt(diag(covmat))
    covmat <- covmat / (std %o% std)
  }

  dimnames(covmat) <- list(colNames, colNames)
  names(center) <- colNames

  if(distance)
    names(dist) <- rowNames

  ans <- list(call = the.call, cov = covmat, center = center, dist = dist, corr = corr)
  oldClass(ans) <- c("covClassic")
  ans
}


