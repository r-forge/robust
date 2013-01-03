fmclass.register <- function(fmclass, classes, validation.function = NULL,
                             attributes.function = NULL)
{
  fmreg <- getOption("fit.models.registry")

  if(fmclass %in% names(fmreg)) {
    warning(fmclass, " is already registered in the fit.models registry")
    return(invisible())
  }

  fmreg[[fmclass]] <- list(fmclass = fmclass,
                           classes = classes,
                           validation.function = validation.function,
                           attributes.function = attributes.function)

  options(fit.models.registry = fmreg)

  invisible()
}


