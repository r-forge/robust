.onAttach <- function(libname, pkgname)
{
  fmreg <- list(fit.models.registry = list())
  attached <- search()
  attach(fmreg, pos = length(attached), name = "data:fit.models.registry")

  fmclass.register(fmclass = "lmfm",
                   classes = c("lm"),
                   validation.function = NULL,
                   attributes.function = NULL)
                                 
  fmclass.register(fmclass = "glmfm",
                   classes = c("glm", "lm"),
                   validation.function = NULL,
                   attributes.function = NULL)

  invisible()
}


