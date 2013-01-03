.onAttach <- function(libname, pkgname)
{
  options(fit.models.registry = list())

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


