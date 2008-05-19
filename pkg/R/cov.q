cov <- function(data, corr = FALSE, center = TRUE, distance = TRUE,
                na.action = na.fail, unbiased = TRUE)
{
	the.call <- match.call()

	data <- na.action(data)
	data <- as.matrix(data)

  n <- dim(data)[1]
	p <- dim(data)[2]

	rowNames <- dimnames(data)[[1]]
	colNames <- dimnames(data)[[2]]
	dimnames(data) <- NULL

	if(is.null(rowNames))
		rowNames <- 1:n

	if(is.null(colNames))
		colNames <- paste("V", 1:p, sep = "")

	if(length(center) != p && is.logical(center)) {
		if(center)
			center <- apply(data, 2, mean)
		else
			center <- rep(0.0, p)
	}

	data <- sweep(data, 2, center)
	covmat <- crossprod(data)

  if(unbiased)
    covmat <-  covmat / (n - 1)
	else
		covmat <-  covmat / n

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
  oldClass(ans) <- c("cov")
  ans
}


