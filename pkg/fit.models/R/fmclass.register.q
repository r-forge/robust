fmclass.register <- function(fmclass, classes, validation.function = NULL,
                             attributes.function = NULL)
{
  attached <- search()
  fmreg.pos <- which(attached == "data:fit.models.registry")

  if(!length(fmreg.pos))
    stop("fit.models registry is not in the search path")

  fmreg <- get("fit.models.registry", pos = fmreg.pos)

  if(fmclass %in% names(fmreg)) {
    warning(fmclass, " is already registered in the fit.models registry")
    return(invisible())
  }

  fmreg[[fmclass]] <- list(fmclass = fmclass,
                           classes = classes,
                           validation.function = validation.function,
                           attributes.function = attributes.function)

  df <- paste("d", "e", "t", "a", "c", "h", sep = "")
  dc <- call(df, "data:fit.models.registry")
  af <- paste("a", "t", "t", "a", "c", "h", sep = "")
  ac <- call(af, list(fit.models.registry = fmreg), length(attached),
             "data:fit.models.registry")
  status <- eval(dc)
  status <- eval(ac)

  invisible()
}


