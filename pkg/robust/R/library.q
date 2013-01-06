.onLoad <- function(libname, pkgname)
{
  library.dynam("robust", package = pkgname, lib.loc = libname)
  invisible()
}


.onAttach <- function(libname, pkgname)
{
  fmclass.add.class("lmfm", "lmRob")
  fmclass.add.class("glmfm", "glmRob")

  fmclass.register("covfm", c("covRob", "covMLE", "cov"))

#  fdfm.attributes.function <- function(model.list, fm.call, attributes)
#  {
#    if(is.null(fm.call[["densfun"]]))
#      distribution <- as.character(fm.call[[4]])
#    else
#      distribution <- as.character(fm.call[["densfun"]])
#
#    if(is.null(fm.call[["x"]]))
#      data.name <- as.character(fm.call[[3]])
#    else
#      data.name <- as.character(fm.call[["x"]])
#
#    x <- get(data.name)
#
#    attrs <- attributes(model.list)
#    attrs[["distribution"]] <- distribution
#    attrs[["data.name"]] <- data.name
#    attrs[["x"]] <- x
#    attributes(model.list) <- attrs
#
#    model.list
#  }
#
#  fm.register.vclass(vclass = "fdfm",
#                     classes = c("fitdistrRob", "fitdistr"),
#                     validation.function = NULL,
#                     attributes.function = fdfm.attributes.function)

  invisible()
}


