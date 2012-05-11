print.fitdistrRob <- function(x, ...)
{
  oldClass(x) <- "fitdistr"
  print(x, ...)
}


