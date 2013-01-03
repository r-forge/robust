fmclass.add.class <- function(fmclass, class, warn = TRUE)
{
  fmreg <- get("fmreg", pos = fit.models:::fm.registry)

  if(class %in% fmreg[[fmclass]]$classes && warn) {
    warning(class, " is already registered in the fit.models registry")
    return(invisible())
  }

  fmreg[[fmclass]]$classes <- union(fmreg[[fmclass]]$classes, class)

  assign("fmreg", fmreg, pos = fit.models:::fm.registry)

  invisible()
}


