fmclass.register <- function(fmclass, classes, validation.function = NULL)
{
  if(fmclass %in% names(e$fmreg)) {
    warning(fmclass, " is already registered in the fit.models registry")
    return(invisible())
  }

  e$fmreg[[fmclass]] <- list(fmclass = fmclass,
                             classes = classes,
                             validation.function = validation.function)

  invisible()
}


