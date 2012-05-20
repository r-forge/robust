#### Tests for covRob()  -*- R -*-
#### ------------------
#### Original :
##
## Author: Kjell Konis
## Date  : 9/1/2000
##

{
    ## Set seed here, as several of the algorithms do random subsampling:
    set.seed(912)

    data(stack.dat) == "stack.dat"
}

### NOTE:  "mcd" is now "fastMCD" from robustbase, see ../../man/covRob.Rd
##  -----> different values below

{
    ## A. test MCD estimator
    temp <- covRob(stack.dat, estim = "mcd")
    covmat <- c(60.9301308800237, 67.3032825008078, 25.1599419754581, 48.989225920203,
                67.3032825008078, 77.3182350477543, 24.0218791860323, 54.9071524252168,
                25.1599419754581, 24.0218791860323, 18.2790392640071, 18.4015998720991,
                48.989225920203, 54.9071524252168, 18.4015998720991, 99.0289713383374)
    locvec <- c(13.1538461538462, 56.1538461538462, 20.2307692307692, 85.3846153846154)
    all(all.equal(as.vector(temp$cov), covmat),
        all.equal(as.vector(temp$center), locvec))
}


{
    ## A1. test MCD estimator: correlation matrix
    temp <- covRob(stack.dat, estim = "mcd", corr = TRUE)
    cormat <- c(1, 0.980571405623223, 0.753905474267168, 0.63067176442357,
                0.980571405623223, 1, 0.638983037772476, 0.627490017608493,
                0.753905474267168, 0.638983037772476, 1, 0.432511606499819,
                0.63067176442357, 0.627490017608493, 0.432511606499819, 1)
    ## locvec  remains the same!
    all(all.equal(as.vector(temp$cov), cormat),
        all.equal(unname(temp$cov), cov2cor(matrix(covmat,4,4)), tol = 1e-14),
        all.equal(as.vector(temp$center), locvec))
}


{
    ## B. test Donoho-Stahel estimator
    temp <- covRob(stack.dat, estim = "donostah")
    covmat <- c(28.1722973969717, 29.4143954121706, 12.02968525837, 17.6045582618356,
                29.4143954121706, 33.9317506099465, 11.2031930724940, 22.1352308669148,
                12.02968525837, 11.2031930724940, 8.29767048720646, 8.79402368139085,
                17.6045582618356, 22.1352308669148, 8.79402368139085, 37.8869805892866)
    locvec <- c(13.7290122612456, 56.9150778381848, 20.4336876111483, 86.286178380903)

    all(all.equal(as.vector(temp$cov), covmat),
        all.equal(as.vector(temp$center), locvec))
}


{
    ## B1. test Donoho-Stahel estimator: correlation matrix
    temp <- covRob(stack.dat, estim = "donostah", corr = TRUE)
    cormat <- c(1, 0.951361694176807, 0.786801248488209, 0.538851383472141,
                0.951361694176807, 1, 0.66766805120483, 0.617356357773397,
                0.786801248488209, 0.66766805120483, 1, 0.495980430114247,
                0.538851383472141, 0.617356357773397, 0.495980430114247, 1)
    ## locvec  remains the same!
    all(all.equal(as.vector(temp$cov), cormat),
        all.equal(unname(temp$cov), cov2cor(matrix(covmat,4,4)), tol = 1e-14),
        all.equal(as.vector(temp$center), locvec))
}


{
    ## C. test M estimator
    temp <- covRob(stack.dat, estim = "M")
    covmat <- c(35.7213914866887, 39.4577669180549, 14.7504711397620, 28.7208199367112,
                39.4577669180549, 45.3292140244877, 14.0832612413037, 32.1903114086941,
                14.7504711397620, 14.0832612413037, 10.7164174460066, 10.7882708196867,
                28.7208199367112, 32.1903114086941, 10.7882708196867, 58.0575259335376)
    locvec <- c(13.1538461538462, 56.1538461538462, 20.2307692307692, 85.3846153846154)
    all(all.equal(as.vector(temp$cov), covmat),
        all.equal(as.vector(temp$center), locvec))
}


{
    ## C1. test M estimator: correaltion matrix
    temp <- covRob(stack.dat, estim = "M", corr = TRUE)
    cormat <- c(1, 0.980571405623223, 0.753905474267168, 0.63067176442357,
                0.980571405623223, 1, 0.638983037772476, 0.627490017608493,
                0.753905474267168, 0.638983037772476, 1, 0.432511606499819,
                0.63067176442357, 0.627490017608493, 0.432511606499819, 1)
    ## locvec  remains the same!
    all(all.equal(as.vector(temp$cov), cormat),
        all.equal(unname(temp$cov), cov2cor(matrix(covmat,4,4)), tol = 1e-14),
        all.equal(as.vector(temp$center), locvec))
}


{
    ## D. test quadrant-correlation estimator
    temp <- covRob(stack.dat, estim = "pairwiseqc")
    covmat <- c(30.6305178882016, 22.2434382439422, 11.2069103829590, 9.82255239190555,
                22.2434382439422, 22.3371486310289, 7.65869436281274, 10.9896726674399,
                11.2069103829590, 7.65869436281274, 6.77696481158792, 4.64292372384087,
                9.82255239190555, 10.9896726674399, 4.64292372384087, 22.0645365958676)
    locvec <- c(14.3125,57.125,20.3125,86.375)
    all(all.equal(as.vector(temp$cov), covmat),
        all.equal(as.vector(temp$center), locvec))
}


{
    ## D1. test quadrant-correlation estimator: correlation matrix
    temp <- covRob(stack.dat, estim = "pairwiseqc", corr = TRUE)
    cormat <- c(1,0.850375778589203,0.777841506522661,0.377832906137698,
                0.850375778589203,1,0.622476923770166,0.495020842047739,
                0.777841506522661,0.622476923770166,1,0.379687699883006,
                0.377832906137698,0.495020842047739,0.379687699883006,1)
    ## locvec  remains the same!
    all(all.equal(as.vector(temp$cov), cormat),
        all.equal(unname(temp$cov), cov2cor(matrix(covmat,4,4)), tol = 1e-14),
        all.equal(as.vector(temp$center), locvec))
}


{
    ## E. test GK estimator
    temp <- covRob(stack.dat, estim = "pairwisegk")
    covmat <- c(17.0869329181457, 17.4702935925913, 7.57137332030014, 9.24173054467015,
                17.4702935925913, 20.7014764200611, 6.3802169389871, 10.8162476004288,
                7.57137332030014,  6.3802169389871, 5.81430356242458, 4.49536028962968,
                9.24173054467015, 10.8162476004288, 4.49536028962968, 22.1345151316791)
    locvec <- c(13.4,56.8,20.0666666666667,86.3333333333333)
    all(all.equal(as.vector(temp$cov), covmat),
        all.equal(as.vector(temp$center), locvec))
}


{
    ## E1. test GK estimator: correlation matrix
    temp <- covRob(stack.dat, estim = "pairwisegk", corr = TRUE)
    cormat <- c(1,0.928896841428589,0.759615384615385,0.475210729999254,
                0.928896841428589,1,0.581548492752874,0.50529029133178,
                0.759615384615385,0.581548492752874,1,0.396260375914721,
                0.475210729999254,0.50529029133178,0.396260375914721,1)
    ## locvec  remains the same!
    all(all.equal(as.vector(temp$cov), cormat),
        all.equal(unname(temp$cov), cov2cor(matrix(covmat,4,4)), tol = 1e-14),
        all.equal(as.vector(temp$center), locvec))
}


{
    ## F. clean up
    rm(temp, covmat, cormat, locvec)
    TRUE
}
