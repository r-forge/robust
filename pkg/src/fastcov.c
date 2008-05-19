#include <S.h>
#include "robust.h"

#define INDEX(i,j,n) (i + j*n)
#define COLUMN(x,col,n) &((x)[col*n])
#define EPSILON 0.00000001

Sfloat robust_location(Sfloat *x, Sint n, Sfloat med, Sfloat mad, Sfloat mu_const);
Sfloat robust_scale(Sfloat *x, Sint n, Sfloat mu, Sfloat mad, Sfloat sigma_const);
Sfloat *eigenvectors_of_U (Sfloat *Y, Sint n, Sint p, Sfloat mu_const, Sfloat sigma_const);
Sfloat *eigenvectors_of_Ut (Sfloat *Y, Sint n, Sint p, Sfloat mu_const, Sfloat sigma_const);
Sfloat mymedian (Sfloat *x, Sint n);
Sfloat mymad (Sfloat *x, Sint n, Sfloat median_x);
extern void F77_NAME(dgemm)();
extern void F77_NAME(rs)();
//extern F77_NAME(dmatmult)();


void rl_fastcov (Sfloat *X, Sint *nn, Sint *pp, Sint *it,
		Sfloat *mu_constant, Sfloat *sigma_constant, 
		Sfloat *V, Sfloat *t, Sfloat *d, Sfloat *median_d,
		Sint *pmethod)
{
  char N = 'N';
	Sfloat dzero = 0.0, done = 1.0;
	Sint ione = 1;
	Sint ii = 0, jj = 0;
  Sint n = *nn, p = *pp, iter = *it, i,j,k, method = *pmethod;
  Sfloat mu_const = *mu_constant, sigma_const = *sigma_constant;
  Sfloat median_j, mad_j;
  Sfloat *mu = Salloc(p, Sfloat);
  Sfloat *sigma = Salloc(p, Sfloat);
  Sfloat *Y = Salloc(n*p, Sfloat);
  Sfloat *D = Salloc(p*p, Sfloat);
  Sfloat *Z = Salloc(n*p, Sfloat);
  Sfloat *A = Salloc(p*p, Sfloat);
  Sfloat *AGamma = Salloc(p*p, Sfloat);
  Sfloat *Gamma = Salloc(p*p, Sfloat);
  Sfloat *nu = Salloc(p, Sfloat);
  Sfloat *E;

  /* Step 1: Form Y = D^-1*X */
  for (j=0; j<p; j++){               /* For each column in XX */ 
    median_j = mymedian (COLUMN(X,j,n), n); 
    mad_j = mymad(COLUMN(X,j,n), n, median_j);
    mu[j] = robust_location( COLUMN(X,j,n), n, median_j, mad_j, mu_const); 
    sigma[j]=robust_scale( COLUMN(X,j,n), n, mu[j], mad_j, sigma_const);

    /* Forming D1, storing in D */
    for (i=0; i<p; i++){
      if (i==j)
        D[INDEX(i,j,p)] = sigma[j];
      else
        D[INDEX(i,j,p)] = 0.0;
    }
    
    /* Forming Y1 */
    for (i=0; i<n; i++)                      /* For each row element in column j */
      Y[INDEX(i,j,n)] = X[INDEX(i,j,n)]/sigma[j];   /*    Y(i,j) = X(i,j)/sigma(j) */
  }
  
  /* Start Iteration */
  for (k=0; k<iter; k++) {
    
    /* STEP 2: Form covariance matrix (U) of Y, find eigenvectors of U (in E) */
		if(method == 0)
			E = eigenvectors_of_U(Y,n,p,mu_const, sigma_const);
		else
			E = eigenvectors_of_Ut(Y,n,p,mu_const, sigma_const);
    
    /* Forming D*E and storing in A*/
    //d1[0]=p; d1[1] = p; d2[0]=p; d2[1]=p;
    //F77_CALL(dmatmult) (D,d1,E,d2,A);
		F77_CALL(dgemm)(&N, &N, &p, &p, &p, &done, D, &p, E, &p, &dzero, A, &p);
 
    /* STEP 3: Z = Y*E */
    //d1[0]=n; d1[1] = p; d2[0]=p; d2[1]=p;
    //F77_CALL(dmatmult) (Y,d1,E,d2,Z);
    F77_CALL(dgemm)(&N, &N, &n, &p, &p, &done, Y, &n, E, &p, &dzero, Z, &n);

    /* Finding mu, sigma of Z. Forming Y=Z/sigma for next iteration.  */
    for (j=0; j<p; j++) {
      median_j = mymedian (COLUMN(Z,j,n), n); 
      mad_j = mymad(COLUMN(Z,j,n), n, median_j);
      mu[j] = robust_location( COLUMN(Z,j,n), n, median_j, mad_j, mu_const); 
      sigma[j]=robust_scale( COLUMN(Z,j,n), n, mu[j], mad_j, sigma_const);
      for (i=0; i<n; i++) 
				Y[INDEX(i,j,n)] = Z[INDEX(i,j,n)]/sigma[j];
    }

    /* Forming Gamma */
    for (i=0; i<p; i++) {
      for (j=0; j<p; j++) {
	if (i==j)
	  Gamma[INDEX(i,j,p)] = sigma[j];
	else
	  Gamma[INDEX(i,j,p)] = 0.0;
      }
    }

    /* Form D*E*Gamma and storing in D*/
		//d1[0]=p; d1[1] = p; d2[0]=p; d2[1]=p;
    //F77_CALL(dmatmult) (A,d1,Gamma,d2,D);
		F77_CALL(dgemm)(&N, &N, &p, &p, &p, &done, A, &p, Gamma, &p, &dzero, D, &p);

  }
  /* End Iteration */
  

  /* So D = D1*E1*Gamma1*E2*Gamma2 ..... Find D_transpose and store in A
   * HERE: A = D_transpose */
  for (i=0; i<p; i++) {
    for (j=0; j<p; j++) {
      A[INDEX(i,j,p)] = D[INDEX(j,i,p)];
    }
  }
  
  /* Form V = D*D_transpose = D*A*/
 // d1[0]=p; d1[1] = p; d2[0]=p; d2[1]=p;
  //F77_CALL(dmatmult) (D,d1,A,d2,V);
	F77_CALL(dgemm)(&N, &N, &p, &p, &p, &done, D, &p, A, &p, &dzero, V, &p);

  /* Form d, mahanlanobis distance */
  for (j=0; j<p; j++) {
    nu[j] = mu[j]/sigma[j];      
    for (i=0; i<n; i++) /* Y = Z/sigma */
      Y[INDEX(i,j,n)] = (Y[INDEX(i,j,n)] - nu[j]) * (Y[INDEX(i,j,n)] - nu[j]);
  }
  for (i=0; i<n; i++){ /* d=row_sum((z/sigma - mu/sigma)^2) */ 
    d[i]=0.0;
    for (j=0; j<p; j++)
      d[i] += Y[INDEX(i,j,n)];
  }

  /* Rescale V = V * median(d) */
  *median_d = mymedian(d,n);
  for (i=0; i<p; i++ ) 
    for (j=0; j<p; j++)
      V[INDEX(i,j,p)] *=  (*median_d);

  /* Form t = D*nu */  
  //d1[0]=p; d1[1] = p; d2[0]=p; d2[1]=1;
  //F77_CALL(dmatmult) (D,d1,nu,d2,t);
	F77_CALL(dgemm)(&N, &N, &p, &ione, &p, &done, D, &p, nu, &p, &dzero, t, &p);

  /* Deallocate memory
  free(Y); free(D); free(Z); free(A); free(Gamma); free(E); free(AGamma); free(mu);
  free(sigma); free(nu);
  */
}


Sfloat robust_location(Sfloat *x, Sint n, Sfloat med, Sfloat mad, Sfloat mu_const){
  Sfloat top = 0.0, mu, bottom = 0.0;
  Sfloat *w = Salloc(n, Sfloat);
  Sint i = 0;

  if( mad > EPSILON) {
    for (i=0; i < n; i++) {  /* w = (1-(x/k)^2)^2  for abs(x) < k=0*/
      w[i] = 1 - ((x[i] - med) / (mad * mu_const)) * ((x[i] - med) / (mad * mu_const));
      if(w[i] < 0) w[i] = 0;
      w[i] = w[i] * w[i];
    }
    for (i=0; i < n; i++) { /* doing the sums */
      top= top + x[i] * w[i];
      bottom = bottom + w[i];
    }
    mu = top / bottom;
  }
	//Free(w);
  return mu;
}


Sfloat robust_scale(Sfloat *x, Sint n, Sfloat mu, Sfloat mad, Sfloat sigma_const){
  Sfloat s = 0.0;
  Sfloat *w = Salloc(n, Sfloat);
  Sint i = 0;
  if( mad > EPSILON) {
    for(i = 0; i < n; i++) {
      w[i] = ((x[i] - mu)*(x[i] - mu)) / (mad * mad);
      if(w[i] > (sigma_const * sigma_const))
	w[i] = sigma_const * sigma_const;
    }
    for(i = 0; i < n; i++) s = s + w[i];
    s = s / n;
    s = mad * sqrt(s);
  }
	//Free(w);
  return s;
}

Sfloat *eigenvectors_of_U (Sfloat *Y, Sint n, Sint p, 
			   Sfloat mu_const, Sfloat sigma_const){

  Sfloat *U = Salloc(p * p, Sfloat);
  Sfloat *eigenvectors = Salloc(p * p, Sfloat);
  Sfloat *eigenvalues = Salloc(p, Sfloat);
  Sfloat *pwork1 = Salloc(p, Sfloat);
  Sfloat *pwork2 = Salloc(p, Sfloat);
  Sfloat *med = Salloc (p, Sfloat);
  Sfloat *mad = Salloc (p, Sfloat);
  Sfloat *S = Salloc (n*p, Sfloat);
  Sint i, j, k, values_only = 1, error_code = 1;
  Sfloat tmp1, tmp2;
  

  /* Finding median and mad for each column in Y*/

    for (j=0; j<p; j++){
         med[j] = mymedian (COLUMN(Y,j,n), n); 
        mad[j] = mymad(COLUMN(Y,j,n), n, med[j]);
       }

    /* Create Sign Matx S*/

    for(i = 0; i < p; i++)
       for(k = 0; k < n; k++)  
	 S[INDEX(k,i,n)] = (Y[INDEX(k,i,n)] < med[i]) ? -1 : 1;

    /* Forming U_jk using Quadrant Correlation */
 
 for(i = 0; i < p; i++) {
    for(j = i; j < p; j++) {
      for(k = 0; k < n; k++) { 
      
      U[INDEX(i, j, p)] += S[INDEX(k,i,n)]*S[INDEX(k,j,n)];
    }
  
  U[INDEX(i, j, p)] = sin((3.1416/2)*U[INDEX(i, j, p)]/n)*mad[i]*mad[j];
  U[INDEX(j, i, p)] = U[INDEX(i, j, p)];
    
  }
 }  
  /* Finding the eigenvalues of U and storing them in matrix eigenvectors */
  F77_CALL(rs) (&p, &p, U, eigenvalues, &values_only, eigenvectors,
      pwork1, pwork2, &error_code);

  /* Reversing the order of eigenvectors (colummwise) */
  for ( j=0; j<(p/2); j++) {
    for (i=0; i<p; i++) {
      tmp1=eigenvectors[INDEX(i,j,p)]; 
      tmp2=eigenvectors[INDEX(i,(p-j-1),p)];
      eigenvectors[INDEX(i,j,p)] =  tmp2;
      eigenvectors[INDEX(i,(p-j-1),p)] = tmp1;
    }
  }

  
/*  Free(U);
  Free(eigenvalues);
  Free(pwork1);
  Free(pwork2);
  Free(med);
  Free(mad);
  Free(S);*/

  return eigenvectors;
}

Sfloat *eigenvectors_of_Ut (Sfloat *Y, Sint n, Sint p, 
			   Sfloat mu_const, Sfloat sigma_const){


  Sfloat *U = Salloc(p * p, Sfloat);
  Sfloat *eigenvectors = Salloc(p * p, Sfloat);
  Sfloat *eigenvalues = Salloc(p, Sfloat);
  Sfloat *pwork1 = Salloc(p, Sfloat);
  Sfloat *pwork2 = Salloc(p, Sfloat);
  Sfloat *u = Salloc(n, Sfloat);
  Sfloat *v = Salloc(n, Sfloat);
  Sint i, j, k, values_only = 1, error_code = 1;
  Sfloat sigma_u, sigma_v, mu_u, mu_v, median_u, median_v, mad_u, mad_v, tmp1, tmp2;
  
  for(i = 0; i < p; i++) {
    for(j = i; j < p; j++) {
       
      /* Find U using Tau estimator*/
      /* u[] = y_j + y_k, v[]= y_j - y_k */
      for(k = 0; k < n; k++) { 
	u[k] = Y[INDEX(k,i,n)] + Y[INDEX(k,j,n)];
	v[k] = Y[INDEX(k,i,n)] - Y[INDEX(k,j,n)];
      }

      median_u =mymedian(u,n);
      median_v =mymedian(v,n);
      mad_u = mymad(u,n, median_u);
      mad_v = mymad(v,n, median_v);

      /* Finding sigma(y_j + y_k), sigma(y_j - y_k) */
      mu_u = robust_location(u, n, median_u, mad_u, mu_const);
      mu_v = robust_location(v, n, median_v, mad_v, mu_const);
      sigma_u = robust_scale(u, n, mu_u, mad_u, sigma_const);
      sigma_v = robust_scale(v, n, mu_v, mad_v, sigma_const);

      /* Forming U_jk= 1/4 (sigma(y_j + y_k)^2 - sigma(y_j - y_k)^2) */
      U[INDEX(i, j, p)] = (sigma_u * sigma_u - sigma_v * sigma_v) / 4;
      U[INDEX(j, i, p)] = U[INDEX(i, j, p)];
    }
  }
  
  /* Finding the eigenvalues of U and storing them in matrix eigenvectors */
  F77_CALL(rs) (&p, &p, U, eigenvalues, &values_only, eigenvectors,
      pwork1, pwork2, &error_code);

  /* Reversing the order of eigenvectors (colummwise) */
  for ( j=0; j<(p/2); j++) {
    for (i=0; i<p; i++) {
      tmp1=eigenvectors[INDEX(i,j,p)]; 
      tmp2=eigenvectors[INDEX(i,(p-j-1),p)];
      eigenvectors[INDEX(i,j,p)] =  tmp2;
      eigenvectors[INDEX(i,(p-j-1),p)] = tmp1;
    }
  }

/*  Free(U);
  Free(eigenvalues);
  Free(u);
  Free(v);
  Free(pwork1);
  Free(pwork2); */

  return eigenvectors;
}

static Sfloat kthplace(Sfloat *a, Sint n, Sint k) {
  Sint jnc,j,l,lr;
  Sfloat ax,w;
  k--;
  l=0;
  lr=n-1;
  while (l<lr)
    { ax=a[k];
    jnc=l;
    j=lr;
    while (jnc<=j) {
      while (a[jnc] < ax) jnc++;
      while (a[j] > ax) j--;
      if (jnc <= j) {
	w=a[jnc];
	a[jnc]=a[j];
	a[j]=w;
	jnc++;
	j--;
      };
    };
    if (j<k) l=jnc;
    if (k<jnc) lr=j;
    };
  return(a[k]);
}

Sfloat mymedian(Sfloat *x, Sint n) {
  Sfloat *aux, t;
  register Sint i = 0;
  
  /* get a "local copy" of the vector in *x 
   * because kthplace() changes the order of its
   * argument
   */

  aux = Salloc(n, Sfloat);

  for(i = 0; i < n ; i++) aux[i] = x[i];

  /* decide which position is the median,
   * or the average of the two "middle" positions
   */
  if (n & 1)
    t = ( kthplace(aux,n,n/2) + kthplace(aux,n,n/2+1) ) / 2 ;
  else t = kthplace(aux,n, n/2+1 ) ;
  
  //Free(aux);
  
  return(t);
}

Sfloat mymad (Sfloat *x, Sint n, Sfloat median_x){
  Sfloat mad_x, *xtmp;
  Sint i = 0;

  xtmp = Salloc(n, Sfloat);

  for (i = 0; i < n; i++)
    xtmp[i] = fabs( x[i]-median_x );

  mad_x = mymedian(xtmp,n)/0.6745;

  //Free(xtmp);
  return mad_x;
}
















