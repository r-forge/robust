arg.names <- function(x) 
{
  switch(mode(x),
  "function" = names(x)[ - length(x)],
  call = names(x)[-1],
  character = {
    x <- getFunction(x)
    names(x)[ - length(x)]
  }
  ,
  stop("mode must be 'function', 'call', or 'character'")
  )
}
