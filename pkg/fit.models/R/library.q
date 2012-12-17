.onAttach <- function(libname, pkgname)
{
  fmreg <- list(fit.models.registry = list())
  attached <- search()
  attach(fmreg, pos = length(attached), name = "data:fit.models.registry")

  registerFMclass(fmclass = "lmfm",
                  classes = c("lm"),
                  validation.function = NULL,
                  attributes.function = NULL)
                                 
  registerFMclass(fmclass = "glmfm",
                  classes = c("glm", "lm"),
                  validation.function = NULL,
                  attributes.function = NULL)

  invisible()
}


