\name{ssb}

\alias{ssb}

\title{Spatial sorting bias}

\description{
Determine "spatial sorting bias", or the difference between two point data sets in the average distance to the nearest point in a reference dataset. 
}

\usage{
ssb(p, a, reference, lonlat=TRUE, avg=TRUE)
}
 
\arguments{
  \item{p}{two column matrix (x, y) or (longitude/latitude) or SpatialPoints object, for point locations}
  \item{a}{two column matrix (x, y) or (longitude/latitude) or SpatialPoints object, for point locations}
  \item{reference}{as above for reference point locations to which distances are computed}
  \item{lonlat}{ Logical. Use \code{TRUE} if the coordinates are spherical (in degrees), and use \code{FALSE} if they are planar }
  \item{avg}{ Logical. If \code{TRUE} the distances are averaged }
}

\value{
matrix with two values. 'dp': the average distance from a point in \code{p} to the nearest point in \code{reference} and 'da': the average distance from a point in \code{a} to the nearest point in \code{reference}.
Distance is in meters if \code{lonlat=TRUE}, and in mapunits (typically also meters) if \code{lonlat=FALSE}
}

\references{
Hijmans, R.J., 2012. Cross-validation of species distribution models: removing spatial sorting bias and calibration with a null-model. Ecology 93: 679-688. 
}

\author{
Robert J. Hijmans
}

\seealso{\code{\link{pwdSample}}}

\examples{
ref <- matrix(c(-54.5,-38.5, 2.5, -9.5, -45.5, 1.5, 9.5, 4.5, -10.5, -10.5), ncol=2)
p <- matrix(c(-56.5, -30.5, -6.5, 14.5, -25.5, -48.5, 14.5, -2.5, 14.5, 
        -11.5, -17.5, -11.5), ncol=2)
r <- raster()
extent(r) <- c(-110, 110, -45, 45)
r[] <- 1
set.seed(0)
a <- randomPoints(r, n=50)
b <- ssb(p, a, ref)

# distances in km
b / 1000

# an index of spatial sorting bias (1 is no bias, near 0 is extreme bias)
b[1] / b[2]
}

\keyword{spatial}
