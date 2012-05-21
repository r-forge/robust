fm.register.class.in.vclass <- function(vclass, class)
{
  fm.env <- as.environment("package:fit.models")
  fm.reg <- get("fit.models.registry", envir = fm.env)

  if(!is.element(vclass, names(fm.reg))) {
    warning(vclass, " is not registered in the fit.models registry")
    return(invisible())
  }

  classes <- unique(c(fm.reg[[vclass]]$classes, class))
  fm.reg[[vclass]]$classes <- classes

  unlockBinding("fit.models.registry", fm.env)
  assign("fit.models.registry", fm.reg, envir = fm.env)
  lockBinding("fit.models.registry", fm.env)

  invisible()
}


