wt.carroll <- function(u, b) 
  (1.0 - (u/b)^2)^3 * (abs(u) <= b)


wt.huber <- function(u, const = 1.345)
{
  U <- abs(u)
  Ugtc <- U > const
  w <- u
  w[!Ugtc] <- 1.0
  w[Ugtc] <- const / U[Ugtc]
  w
}


