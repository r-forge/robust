fmclass.add.class <- function(fmclass, class, warn = TRUE)
{
  attached <- search()
  fmreg.pos <- which(attached == "data:fit.models.registry")

  if(!length(fmreg.pos))
    stop("fit.models registry is not in the search path")

  fmreg <- get("fit.models.registry", pos = fmreg.pos)

  if(class %in% fmreg[[fmclass]]$classes && warn) {
    warning(class, " is already registered in the fit.models registry")
    return(invisible())
  }

  fmreg[[fmclass]]$classes <- union(fmreg[[fmclass]]$classes, class)

  detach(name = "data:fit.models.registry")
  attach(list(fit.models.registry = fmreg), pos = length(attached),
         name = "data:fit.models.registry")

  invisible()
}


