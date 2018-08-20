\name{circleHull}

\alias{circleHull}
\alias{circleHull,SpatialPoints-method}
\alias{circleHull,matrix-method}
\alias{circleHull,data.frame-method}
\alias{CircleHull-class}


\title{Circle hull model}


\description{
The Circle hull model predicts that a species is present at sites inside the smallest circle that can contain a set of training points, and absent outside that circle.
}


\usage{
circleHull(p, ...)
}
 
\arguments{
  \item{p}{point locations (presence). Two column matrix, data.frame or SpatialPoints* object}
  \item{...}{ Additional arguments. See details }
}


\value{
An object of class 'CircleHull' (inherits from \code{\link{DistModel-class}})
}


\author{
Robert J. Hijmans
}

\seealso{
\code{\link[dismo]{circles}, \link[dismo]{convHull}, \link[dismo]{rectHull}, \link[dismo]{predict}}
}


\examples{
r <- raster(system.file("external/rlogo.grd", package="raster"))
#presence data
pts <- matrix(c(17, 42, 85, 70, 19, 53, 26, 84, 84, 46, 48, 85, 4, 95, 48, 54, 66, 
 74, 50, 48, 28, 73, 38, 56, 43, 29, 63, 22, 46, 45, 7, 60, 46, 34, 14, 51, 70, 31, 39, 26), ncol=2)
train <- pts[1:12, ]
test <- pts[13:20, ]
				 
ch <- circleHull(train)
predict(ch, test)

plot(r)
plot(ch, border='red', lwd=2, add=TRUE)
points(train, col='red', pch=20, cex=2)
points(test, col='black', pch=20, cex=2)

pr <- predict(ch, r, progress='')
plot(pr)
points(test, col='black', pch=20, cex=2)
points(train, col='red', pch=20, cex=2)

# to get the polygons:
p <- polygons(ch)
p
}

\keyword{spatial}

