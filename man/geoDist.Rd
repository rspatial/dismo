\name{Geographic Distance}

\alias{geoDist}
\alias{geoDist,SpatialPoints-method}
\alias{geoDist,matrix-method}
\alias{geoDist,data.frame-method}
\alias{GeographicDistance-class}


\title{Geographic distance model}


\description{
The geographic distance model predicts that the likelyhood of presence is highest near places where a species has been observed. It can be used as a null-model to calibrate cross-validation scores with.

The predicted values are the inverse distance to the nearest known presence point. Distances smaller than or equal to zero are set to 1 (highest score).
}

\usage{
geoDist(p, ...)
}
 
\arguments{
  \item{p}{point locations (presence). Two column matrix, data.frame or SpatialPoints* object}
  \item{...}{ Additional arguments. You must supply a lonlat= argument (logical), unless p is a SpatialPoints* object and has a valid CRS (coordinate reference system). 
  You can also supply an additional argument 'a' for absence points (currently ignored). Argument 'a' should be of the same class as argument 'p' }
}


\value{
An object of class 'GeographicDistance' (inherits from \code{\link{DistModel-class}})
}


\author{
Robert J. Hijmans
}

\seealso{
\code{\link[dismo]{predict}, \link{convHull}, \link{maxent}, \link{domain}, \link{mahal}, \link{voronoiHull}, \link{geoIDW}}
}


\examples{
r <- raster(system.file("external/rlogo.grd", package="raster"))
#presence data
pts <- matrix(c(17, 42, 85, 70, 19, 53, 26, 84, 84, 46, 48, 85, 4, 95, 48, 54, 66, 74, 50, 48, 
        28, 73, 38, 56, 43, 29, 63, 22, 46, 45, 7, 60, 46, 34, 14, 51, 70, 31, 39, 26), ncol=2)
colnames(pts) <- c('x', 'y')

train <- pts[1:12, ]
test <- pts[13:20, ]
				 
gd <- geoDist(train, lonlat=FALSE)
p <- predict(gd, r)

\dontrun{
plot(p)
points(test, col='black', pch=20, cex=2)
points(train, col='red', pch=20, cex=2)
}
}

\keyword{spatial}

