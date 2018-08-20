\name{randomPoints}
\alias{randomPoints}

\title{
Random points
}

\description{
Generate random points that can be used to extract background values ("random-absence"). The points are sampled (without replacement) from the cells that are not '\code{NA}' in raster '\code{mask}'. 

If the coordinate reference system (of \code{mask}) is longitude/latitude, sampling is weighted by the size of the cells. That is, because cells close to the equator are larger than cells closer to the poles, equatorial cells have a higher probability of being selected.
}

\usage{
randomPoints(mask, n, p, ext=NULL, extf=1.1, excludep=TRUE, prob=FALSE, 
             cellnumbers=FALSE, tryf=3, warn=2, lonlatCorrection=TRUE)
}
 
\arguments{
  \item{mask}{Raster* object. If the object has cell values, cells with \code{NA} are excluded (of the first layer of the object if there are multiple layers)}
  \item{n}{integer. Number of points}
  \item{p}{Presence points (if provided, random points won't be in the same cells (as defined by mask)}
  \item{ext}{\link{Extent} object. Can be used to restrict sampling to a spatial extent}
  \item{extf}{numeric. Multiplyer to adjust the size of extent 'ext'. The default increases of 1.1 increases the extent a little (5\% at each side of the extent)}
  \item{excludep}{logical. If \code{TRUE}, presence points are exluded from background}
  \item{prob}{logical. If \code{TRUE} the values in \code{mask} are interpreted as probability weights (and the values should be positive numbers (or NA)). NOTE:  this currently only works for rasters of a relatively modest size (that can be loaded into RAM)}
  \item{cellnumbers}{logical. If \code{TRUE}, cell numbers for ]code{mask} are returned rather than coordinates }
  \item{tryf}{numeric > 1. Multiplyer used for initial sample size from which the requested sample size is extracted after removing NA points (outside of mask) }
  \item{warn}{integer. 2 or higher gives most warnings. 0 or lower gives no warnings if sample size \code{n} is not reached }
  \item{lonlatCorrection}{logical. If \code{TRUE} then correct for the fact that longitude/latitude is not a planar coordinate system}
}

\value{
matrix with coordinates, or, if \code{cellnumbers=TRUE}, a vector with cell numbers.
}

\author{
Robert J. Hijmans
}


\keyword{spatial}

