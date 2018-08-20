/* Robert Hijmans, October 2012 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>

#define min( a, b ) ( ((a) < (b)) ? (a) : (b) )


SEXP _percRank(SEXP x, SEXP dimx, SEXP y, SEXP dimy, SEXP minc, SEXP maxc, SEXP tail) {
					
	R_len_t v, i, ii, j, jj;
	SEXP val;
	double *px, *py, *pval, *pminc, *pmaxc, z;
	int *ptail, b, t, tailopt;
	
	PROTECT(x = coerceVector(x, REALSXP));
	PROTECT(y = coerceVector(y, REALSXP));
	PROTECT(minc = coerceVector(minc, REALSXP));
	PROTECT(maxc = coerceVector(maxc, REALSXP));
	PROTECT(tail = coerceVector(tail, INTSXP));
	int nrowx = INTEGER(dimx)[0];
	int ncolx = INTEGER(dimx)[1];
	int nrowy = INTEGER(dimy)[0];
//	int ncoly = INTEGER(dimy)[1];
	
	px = REAL(x);
	py = REAL(y);
	pminc = REAL(minc);
	pmaxc = REAL(maxc);
	ptail = INTEGER(tail);

	PROTECT( val = allocVector(REALSXP, nrowy) );
	pval = REAL(val);
	for (i=0; i<nrowy; i++) {
		pval[i] = 100;
	}
	
	for (v=0; v<ncolx; v++) {
		//Rprintf ("v = %i \n", v);
		tailopt = ptail[v];
		for (ii=0; ii<nrowy; ii++) {
			i = ii + v * nrowy;
			if ( R_FINITE(pval[ii]) ) {
				if (! R_FINITE(py[i]) ) {
					pval[ii] = R_NaReal;
				} else if ((py[i] < pminc[v]) | (py[i] > pmaxc[v] )) {
					pval[ii] = 0; 
				} else {
					b = 0;
					t = 0;
					for (jj=0; jj<nrowx; jj++) {
						j = jj + v * nrowx;
						if (py[i] > px[j]) {
							b++;
						} else if (py[i] == px[j]) {
							t++;
						} else {
						// the columns of y are sorted, so we need not continue
							break;
						}
					}
					z = (b + 0.5 * t) / nrowx;
					if (tailopt == 1) { // both
						if (z > 0.5) {
							z = 2 * (1 - z); 
						} else {
							z = 2 * z;
						}
					
					} else if (tailopt == 2) { // high
						if (z < 0.5) {
							z = 1;
						} else {
							z = 2 * (1 - z);
						}
					} else if (tailopt == 3) { // low
						if (z > 0.5) {
							z = 1;
						} else {
							z = 2 * z;
						}
					}
					pval[ii] = min(pval[ii], z);
				} 
			}
		}
	}
	UNPROTECT(6);
	return(val);
}


