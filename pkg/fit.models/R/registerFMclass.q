registerFMclass <- function(fmclass, classes, validation.function = NULL,
                            attributes.function = NULL)
{
  attached <- search()
  fmr.pos <- which(attached == "data:fit.models.registry")

  if(!length(fmr.pos))
    stop("fit.models registry is not in the search path")

  fmreg <- get("fit.models.registry", pos = fmr.pos)
  detach(name = "data:fit.models.registry")

  if(fmclass %in% names(fmreg)) {
    warning(fmclass, " is already registered in the fit.models registry")
    return(invisible())
  }

  fmreg[[fmclass]] <- list(classes = classes,
                           object.class = fmclass,
                           validation.function = validation.function,
                           attributes.function = attributes.function)

  attach(list(fit.models.registry = fmreg), pos = length(attached),
         name = "data:fit.models.registry")

  invisible()
}


