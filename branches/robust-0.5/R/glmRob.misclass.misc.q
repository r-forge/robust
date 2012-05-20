glmRob.misclass.F <- function(u)
  1 / (1 + exp(-u))

glmRob.misclass.f <- function(u)
  exp(-u) / (( 1 +exp(-u))^2)

glmRob.misclass.G <- function(u, lambda) 
  glmRob.misclass.F(u) + lambda * (1.0 - 2.0 * glmRob.misclass.F(u))

glmRob.misclass.g <- function(u, lambda) 
  glmRob.misclass.f(u) - 2.0 * lambda * glmRob.misclass.f(u)

glmRob.misclass.w <- function(u, lambda) 
  ((1.0 - 2.0 * lambda) * glmRob.misclass.F(u) * (1.0 - glmRob.misclass.F(u))) / 
    (glmRob.misclass.G(u, lambda) * (1.0 - glmRob.misclass.G(u, lambda)))

