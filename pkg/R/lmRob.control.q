lmRob.control <- function(tlo = 0.0001, tua = 1.5e-06, mxr = 50,
    mxf = 50, mxs = 50, tl = 1e-6, estim = "Final", initial.alg = "Auto",
    final.alg = "MM", seed = 1313, level = 0.10, efficiency = 0.90,
    weight = c("Optimal","Optimal"), trace = TRUE)
{
  list(tlo = tlo, tua = tua, mxr = mxr, mxf = mxf, mxs = mxs, tl = tl,
       estim = estim, initial.alg = initial.alg, final.alg = final.alg, 
       seed = seed, level = level, efficiency = efficiency, weight = weight,
       trace = trace)
}


