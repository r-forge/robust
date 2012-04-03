.onLoad <- function(libname, pkgname)
{
  library.dynam("robust", package = pkgname, lib.loc = libname)
  invisible()
}

