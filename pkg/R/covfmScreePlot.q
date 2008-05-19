covfmScreePlot <- function(x, variables, ...)
{
	n.models <- length(x)
	mod.names <- names(x)
  
  eval.extractor <- function(u)
    eigen(u$cov, symmetric = TRUE, only.values = TRUE)$values				
  evals <- matrix(sapply(x, eval.extractor), ncol = n.models)

  if(missing(variables))
    variables <- 1:min(10, dim(evals)[1])

  matplot(1:length(variables),
    evals[variables, , drop = FALSE],
    type = "o",
    axes = FALSE,
    xlab = "",
    ylab = "Eigenvalues",	
    lty = 1:n.models,
    pch = 1:n.models,
    col = 1:n.models,
    ...)

  axis(2)
  axis(1, at = 1:length(variables), labels = paste("Eval.", variables))

  key(text = list(mod.names),
    lines = list(type = "o",
                 pch = 1:n.models,
                 lty = 1:n.models,
                 col = 1:n.models),
    transparent = TRUE)

  invisible(x)
}


