get.fit.models.registry <- function()
{
  fmreg.pos <- which(search() == "data:fit.models.registry")
  get("fit.models.registry", pos = fmreg.pos)
}


