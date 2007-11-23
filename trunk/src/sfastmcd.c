#include <S.h>
#include "robust.h"

extern void F77_NAME(sfastmcd)(Sfloat* dat, Sint* n, Sint* nvar,
	Sint* nhalff, Sint* krep, Sfloat* initcov, Sfloat* initmean,
	Sint* inbest, Sfloat* det, Sfloat* weight, Sfloat* fit,
	Sfloat* plane, Sint* kount, Sfloat* adcov, Sint* nmax,
	Sint* temp, Sint* index1, Sint* index2, Sfloat* nmahad,
	Sfloat* ndist, Sfloat* am, Sfloat* am2, Sfloat* slutn);

void robust_fastmcd(Sfloat* dat, Sint* n, Sint* nvar, Sint* nhalff,
  Sint* krep, Sfloat* initcov, Sfloat* initmean, Sint* inbest,
	Sfloat* det, Sfloat* weight, Sfloat* fit, Sfloat* plane,
	Sint* kount, Sfloat* adcov, Sint* nmax)
{
	Sint *temp, *index1, *index2 ;
	Sfloat *nmahad, *ndist, *am, *am2, *slutn ;

	temp = Salloc(*nmax, Sint);
	index1 = Salloc(*nmax, Sint);
	index2 = Salloc(*nmax, Sint);
	nmahad = Salloc(*nmax, Sfloat);
	ndist = Salloc(*nmax, Sfloat);
	am = Salloc(*nmax, Sfloat);
	am2 = Salloc(*nmax, Sfloat);
	slutn = Salloc(*nmax, Sfloat);

	F77_CALL(sfastmcd)(dat, n, nvar, nhalff, krep, initcov,
	  initmean, inbest, det, weight, fit, plane, kount, adcov, nmax,
		temp, index1, index2, nmahad, ndist, am, am2, slutn) ;
}

