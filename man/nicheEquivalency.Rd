\name{nicheEquivalency}

\alias{nicheEquivalency}

\title{Niche equivalency}

\description{
Compute niche equivalency for two species following Warren et al. (2009). The statistic ranges from 0 to 1.
}

\usage{ 
nicheEquivalency(sp1, sp2, predictors, n=99, model=maxent, verbose=TRUE, ...) 
}


\arguments{
\item{sp1}{coordinates for species 1 (matrix with (x, y) or (lon, lat), or SpatialPoints)}
\item{sp2}{coordinates for species 2 (matrix with (x, y) or (lon, lat), or SpatialPoints)}
\item{predictors}{Raster object with environmental variables}
\item{n}{integer. Number of randomizations}
\item{model}{function. modeling algorithm to me used}
\item{verbose}{logical. If \code{TRUE} some progress indicators are printed}
\item{...}{additional arguments (none)}
}

\value{numeric}

\author{Brian Anacker. Based on a similar function in by Christoph Heibl in the phyloclim package}

\references{ Warren, D.L., R.E. Glor, M. Turelli, 2008. Environmental niche equivalency versus conservatism: quantitative approaches to niche evolution. Evolution 62:2868-2883. }

