glmRob.control <- function(method, ...)
{
  switch(method,
    "cubif" = glmRob.cubif.control(...),
    "mallows" = glmRob.mallows.control(...),
    "misclass" = glmRob.misclass.control(...)
  )
}

