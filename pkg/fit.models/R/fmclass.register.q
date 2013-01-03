fmclass.register <- function(fmclass, classes, validation.function = NULL,
                             attributes.function = NULL)
{
  fmreg <- get("fmreg", pos = fit.models:::fm.registry)

  if(fmclass %in% names(fmreg)) {
    warning(fmclass, " is already registered in the fit.models registry")
    return(invisible())
  }

  fmreg[[fmclass]] <- list(fmclass = fmclass,
                           classes = classes,
                           validation.function = validation.function,
                           attributes.function = attributes.function)

  assign("fmreg", fmreg, pos = fit.models:::fm.registry)

  invisible()
}




