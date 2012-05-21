fm.register.vclass <- function(vclass, classes, validation.function = NULL,
                                 attributes.function = NULL)
{
  fm.env <- as.environment("package:fit.models")
  fm.reg <- get("fit.models.registry", envir = fm.env)

  if(vclass %in% names(fm.reg)) {
    warning(vclass, " is already registered in the fit.models registry")
    return(invisible())
  }

  fm.reg[[vclass]] <- list(classes = classes,
                         object.class = vclass,
                         validation.function = validation.function,
                         attributes.function = attributes.function)

  unlockBinding("fit.models.registry", fm.env)
  assign("fit.models.registry", fm.reg, envir = fm.env)
  lockBinding("fit.models.registry", fm.env)

  invisible()
}


