.onLoad <- function(libname, pkgname)
{
  e$fmreg <- list()

  fmclass.register(fmclass = "lmfm",
                   classes = c("lm"),
                   validation.function = NULL)

  fmclass.register(fmclass = "glmfm",
                   classes = c("glm", "lm"),
                   validation.function = NULL)
  invisible()
}

## hopefully we do not need this "onAttach" !  --
if(FALSE)
.onAttach <- function(libname, pkgname)
{
  assign("fmreg", list(), envir = e)

  fmclass.register(fmclass = "lmfm",
                   classes = c("lm"),
                   validation.function = NULL)

  fmclass.register(fmclass = "glmfm",
                   classes = c("glm", "lm"),
                   validation.function = NULL)

  invisible()
}


