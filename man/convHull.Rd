\name{Convex Hull}

\alias{convHull}
\alias{convHull,SpatialPoints-method}
\alias{convHull,matrix-method}
\alias{convHull,data.frame-method}
\alias{ConvexHull-class}


\title{Convex hull model}


\description{
The Convex hull model predicts that a species is present at sites inside the convex hull of a set of training points, and absent outside that hull.
I.e. this is the spatial convex hull, not an environmental hull.
}


\usage{
convHull(p, ...)
}
 
\arguments{
  \item{p}{point locations (presence). Two column matrix, data.frame or SpatialPoints* object}
  \item{...}{ Additional arguments. See details }
}


\details{
You can supply an argument n (>= 1) to get n convex hulls around subsets of the points. You can also set n=1:x, to get a set of overlapping polygons consisting of 1 to x parts. I.e. the first polygon has 1 part, the second has 2 parts, and x has x parts.
}


\value{
An object of class 'ConvexHull' (inherits from \code{\link{DistModel-class}})
}


\author{
Robert J. Hijmans
}

\seealso{
\code{\link[dismo]{predict}, \link[dismo]{geoDist}, \link[dismo]{maxent}, \link[dismo]{domain}, \link[dismo]{mahal}}
}


\examples{
r <- raster(system.file("external/rlogo.grd", package="raster"))
#presence data
pts <- matrix(c(17, 42, 85, 70, 19, 53, 26, 84, 84, 46, 48, 85, 4, 95, 48, 54, 66, 
 74, 50, 48, 28, 73, 38, 56, 43, 29, 63, 22, 46, 45, 7, 60, 46, 34, 14, 51, 70, 31, 39, 26), ncol=2)
train <- pts[1:12, ]
test <- pts[13:20, ]
				 
ch <- convHull(train)
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

