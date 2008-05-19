aovRob <- function(formula, data, contrasts = NULL, ...)
{
  the.call <- match.call()

  if(!missing(data))
    m <- model.frame(formula, data)
  else
    m <- model.frame(formula)

  m <- m[, -1, drop = FALSE]
  m <- apply(m, 1, paste, collapse = "")
  balanced <- length(unique(table(m))) == 1

  if(missing(data))
    Terms <- terms(formula, "Error") 
  else 
    Terms <- terms(formula, "Error", data = data)

  if(!inherits(formula, "formula"))
    formula <- attr(Terms, "formula")

  if(!is.null(attr(Terms, "specials")$Error))
    warning("Error is not supported in aovRob().")

  lmcall <- the.call
  lmcall[[1]] <- as.name("lmRob")
  lmcall$formula <- Terms
  result <- eval(lmcall, sys.parent())

  result$call <- the.call
  result$balanced <- balanced
  oldClass(result) <- "aovRob"
  result
}


