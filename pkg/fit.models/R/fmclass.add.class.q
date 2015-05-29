fmclass.add.class <- function(fmclass, class, warn = TRUE)
{
  if(class %in% e$fmreg[[fmclass]]$classes && warn) {
    warning(class, " is already registered in the fit.models registry")
    return(invisible())
  }

  e$fmreg[[fmclass]]$classes <- union(e$fmreg[[fmclass]]$classes, class)

  invisible()
}


