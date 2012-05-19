# quick test for princompRob
{
	temp <- princompRob(stack.dat, estim = "mcd")

	all.equal(as.vector(temp$sdev), c(10.1604100898979,
		4.70159223508389,2.23466903449916,0.446761914065813),
		tolerance = 1.0e-5)
}


{
	temp <- princompRob(woodmod.dat, estim = "donostah", tune = .90,
					prob = .999, eps = .3)

	all.equal(as.vector(temp$sdev), c(0.130869619858481,0.0779173556583895,
		0.0754135389664265,0.0494762031133952,0.0123680308004224),
		tolerance = 1.0e-5)
}


{
	rm(temp)
	T
}

