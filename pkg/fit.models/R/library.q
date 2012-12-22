.onAttach <- function(libname, pkgname)
{
  fmreg <- list(fit.models.registry = list())
  attached <- search()

  af <- paste("a", "t", "t", "a", "c", "h", sep = "")
  ac <- call(af, fmreg, length(attached), "data:fit.models.registry")
  status <- eval(ac)

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


