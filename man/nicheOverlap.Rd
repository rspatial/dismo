\name{nicheOverlap}

\alias{nicheOverlap}

\title{Niche overlap}

\description{
Compute niche overlap from predictions of species distributions with the 'I' or 'D' similarity statistic of Warren et al. (2009). The statistic ranges from 0 (no overlap) to 1 (the distributions are identical).
}

\usage{ nicheOverlap(x, y, stat='I', mask=TRUE, checkNegatives=TRUE) }

\arguments{
\item{x}{RasterLayer with non-negative values (predictions of the probability that a site is suitable for a species)}
\item{y}{RasterLayer with non-negative values, as above}
\item{stat}{character either 'I' or 'D' to get the statistic with that name}
\item{mask}{logical. If \code{TRUE} the function removes cells from \code{x} that are \code{NA} in \code{y} and vice-versa. If you are sure that such cases do not occur you can set this to \code{FALSE} to speed up computations}
\item{checkNegatives}{logical. If \code{TRUE} the function checks of any of the values in \code{x} and \code{y} are negative. If you are sure that such cases do not occur you can set this to \code{FALSE} to speed up computations}
}

\value{numeric}

\author{Based on SDMTools::Istat by Jeremy VanDerWal }

\references{ Warren, D.L., R.E. Glor, M. Turelli, and D. Funk. 2009. Environmental niche equivalency versus conservatism: quantitative approaches to niche evolution. Evolution 62:2868-2883; Erratum: Evolution 65: 1215}


\examples{
r1 <- raster(nr=18, nc=36)
r2 <- raster(nr=18, nc=36)
set.seed(0)
r1[] <- runif(ncell(r1))
r2[] <- runif(ncell(r1))
nicheOverlap(r1, r2)
}
