covfmScreePlot <- function(x, variables, main, xlab, ylab, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  if(missing(main))
    main <- ""

  if(missing(xlab))
    xlab <- ""

  if(missing(ylab))
    ylab <- "Eigenvalues"
  
  eval.extractor <- function(u)
    eigen(u$cov, symmetric = TRUE, only.values = TRUE)$values
  evals <- matrix(sapply(x, eval.extractor), ncol = n.models)

  if(missing(variables))
    variables <- 1:min(10, dim(evals)[1])

  matplot(1:length(variables),
          evals[variables, , drop = FALSE],
          type = "o",
          axes = FALSE,
          main = main,
          xlab = xlab,
          ylab = ylab,  
          lty = 1:n.models,
          pch = 1:n.models,
          col = 1:n.models,
          ...)

  axis(2)
  axis(1, at = 1:length(variables), labels = paste("Eval.", variables))

  legend(x = "topright", legend = mod.names, pch = 1:n.models, lty = 1:n.models,
         col = 1:n.models, bty = "n")

  invisible(x)
}


