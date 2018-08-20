\name{CirclesRange}

\alias{circles}
\alias{circles,SpatialPoints-method}
\alias{circles,matrix-method}
\alias{circles,data.frame-method}
\alias{CirclesRange-class}


\title{Circles range}


\description{
The Circles Range model predicts that a species is present at sites within a certain distance from a training point, and absent further away.
}


\usage{
\S4method{circles}{matrix}(p, d, lonlat, n=360, r=6378137, dissolve=TRUE, ...)

\S4method{circles}{SpatialPoints}(p, d, lonlat, n=360, r=6378137, dissolve=TRUE, ...)
}
 
\arguments{
  \item{p}{point locations (presence). Two column matrix, data.frame or SpatialPoints* object}
 \item{d}{numeric. The radius of each circle in meters. A single number or a vector with elements corresponding to rows in \code{p}. If missing the diameter is computed from the mean inter-point distance}
 \item{lonlat}{logical. Are these longitude/latitude data? If missing this is taken from the \code{p} if it is a SpatialPoints* object}
 \item{n}{integer. How many vertices in the circle? Default is 360}
  \item{r}{numeric. Radius of the earth. Only relevant for longitude/latitude data. Default is 6378137 m}
  \item{dissolve}{logical. Dissolve overlapping circles. Setting this to \code{FALSE} may be useful for plotting overlapping circles} 
  \item{...}{additional arguments, none implemented}
} 


\value{
An object of class 'CirclesRange' (inherits from \code{\link{DistModel-class}})
}

\author{
Robert J. Hijmans
}

\seealso{
\code{\link[dismo]{predict}, \link[dismo]{geoDist}, \link[dismo]{convHull},  \link[dismo]{maxent}, \link[dismo]{domain}, 
\link[dismo]{mahal}, \link[dismo]{convHull}  }
}


\examples{
r <- raster(system.file("external/rlogo.grd", package="raster"))
#presence data
pts <- matrix(c(17, 42, 85, 70, 19, 53, 26, 84, 84, 46, 48, 85, 4, 95, 48, 54, 66,
 74, 50, 48, 28, 73, 38, 56, 43, 29, 63, 22, 46, 45, 7, 60, 46, 34, 14, 51, 70, 31, 39, 26), ncol=2)
train <- pts[1:12, ]
test <- pts[13:20, ]
				 
cc <- circles(train, lonlat=FALSE)
predict(cc, test)

plot(r)
plot(cc, border='red', lwd=2, add=TRUE)
points(train, col='red', pch=20, cex=2)
points(test, col='black', pch=20, cex=2)

pr <- predict(cc, r, progress='')
plot(r, legend=FALSE)
plot(pr, add=TRUE, col='blue')
points(test, col='black', pch=20, cex=2)
points(train, col='red', pch=20, cex=2)


# to get the polygons:
p <- polygons(cc)
p

# to compute the Circular Area Range of a species (see Hijmans and Spooner, 2001)
pts <- train*10
ca100 <- polygons(circles(pts, d=100, lonlat=FALSE))
ca150 <- polygons(circles(pts, d=150, lonlat=FALSE))
sum(area(ca150)) / (pi*150^2)
sum(area(ca100)) / (pi*100^2)
par(mfrow=c(1,2))
plot(ca100); points(pts)
plot(ca150); points(pts)



}

\keyword{spatial}

