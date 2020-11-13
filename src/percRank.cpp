/* Robert Hijmans, October 2012 */

#include <Rcpp.h>

// [[Rcpp::export(name = ".percRankCpp")]]
std::vector<double> percRank(std::vector<double> x, std::vector<unsigned> dimx, std::vector<double> y, std::vector<unsigned> dimy, std::vector<double> minc, std::vector<double> maxc, std::vector<int> tail) {
					
	size_t nrowx = dimx[0];
	size_t ncolx = dimx[1];
	size_t nrowy = dimy[0];

	std::vector<double> out(nrowy, 100);
	
	for (size_t v=0; v<ncolx; v++) {
		int tailopt = tail[v];
		for (size_t ii=0; ii<nrowy; ii++) {
			size_t i = ii + v * nrowy;
			if (!std::isnan(out[ii]) ) {
				if (std::isnan(y[i]) ) {
					out[ii] = NAN;
				} else if ((y[i] < minc[v]) | (y[i] > maxc[v] )) {
					out[ii] = 0; 
				} else {
					size_t b = 0;
					size_t t = 0;
					for (size_t jj=0; jj<nrowx; jj++) {
						size_t j = jj + v * nrowx;
						if (y[i] > x[j]) {
							b++;
						} else if (y[i] == x[j]) {
							t++;
						} else {
						// the columns of y are sorted, so we need not continue
							break;
						}
					}
					double z = (b + 0.5 * t) / nrowx;
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
					out[ii] = std::min(out[ii], z);
				} 
			}
		}
	}
	return(out);
}


