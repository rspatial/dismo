\name{InvDistW}
\Rdversion{1.1}

\alias{geoIDW}
\alias{geoIDW,SpatialPoints,SpatialPoints-method}
\alias{geoIDW,matrix,matrix-method}
\alias{geoIDW,data.frame,data.frame-method}
\alias{InvDistWeightModel-class}


\title{Inverse-distance weighted model}

\description{
Inverse-distance weighted predictions for presence/absence data. Computed with the gstat function from the gstat package.
}

\usage{
geoIDW(p, a, ...)
}

\arguments{
 \item{p}{Presence points. Two column matrix, data.frame, or a SpatialPoints* object}
 \item{a}{Absence points. Must be of the same class as p}
 \item{...}{Addtional arguments. None implemented}
}

\value{
An object of class InvDistWeightModel (inherits from \code{\link{DistModel-class}})
}

\author{
Robert J. Hijmans
}

\examples{
r <- raster(system.file("external/rlogo.grd", package="raster"))
# presence points
p <- matrix(c(17, 42, 85, 70, 19, 53, 26, 84, 84, 46, 48, 85, 4, 95, 48, 54, 66, 74, 50, 48, 
      28, 73, 38, 56, 43, 29, 63, 22, 46, 45, 7, 60, 46, 34, 14, 51, 70, 31, 39, 26), ncol=2)

# absence points
a <- matrix(c(30, 23, 5, 5, 31, 33, 91, 63, 60, 88, 93, 97, 65, 68, 85, 97, 35, 32, 29, 55,
      3, 8, 19, 71, 49, 36, 69, 41, 20, 28, 18, 9, 5, 9, 25, 71, 8, 32, 46, 60), ncol=2)

idw <- geoIDW(p, a)
prd <- predict(r, idw)

\dontrun{
plot(prd)
points(p)
points(a, pch='x')
}
}

\keyword{ spatial  }

