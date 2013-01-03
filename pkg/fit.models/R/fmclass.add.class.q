fmclass.add.class <- function(fmclass, class, warn = TRUE)
{
  fmreg <- getOption("fit.models.registry")

  if(class %in% fmreg[[fmclass]]$classes && warn) {
    warning(class, " is already registered in the fit.models registry")
    return(invisible())
  }

  fmreg[[fmclass]]$classes <- union(fmreg[[fmclass]]$classes, class)

  options(fit.models.registry = fmreg)

  invisible()
}


