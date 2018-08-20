\name{mahal}

\alias{mahal}
\alias{mahal,Raster,SpatialPoints-method}
\alias{mahal,Raster,matrix-method}
\alias{mahal,Raster,data.frame-method}
\alias{mahal,matrix,missing-method}
\alias{mahal,data.frame,missing-method}
\alias{Mahalanobis-class}


\title{
Mahalanobis model
}

\description{
Distribution model based on the Mahalanobis distance. The predictions are (1-distance). I.e. the highest possible value is 1, and there will likely be large negative numbers.
}

\usage{
mahal(x, p, ...)
}
 
\arguments{
  \item{x}{ Raster* object or matrix}
  \item{p}{ two column matrix or SpatialPoints* object}
  \item{...}{ Additional arguments. Currently not used }
}


\value{
An object of class Mahalanobis (inherits from \code{\link{DistModel-class}})
}


\seealso{
\code{\link[dismo]{predict}, \link[dismo]{maxent}, \link[dismo]{bioclim}, \link[dismo]{domain}}
}


\author{
Robert J. Hijmans
}

\examples{
logo <- stack(system.file("external/rlogo.grd", package="raster"))

#presence data
pts <- matrix(c(48.243420, 48.243420, 47.985820, 52.880230, 49.531423, 46.182616, 
  54.168232, 69.624263, 83.792291, 85.337894, 74.261072, 83.792291, 95.126713, 
  84.565092, 66.275456, 41.803408, 25.832176, 3.936132, 18.876962, 17.331359, 
  7.048974, 13.648543, 26.093446, 28.544714, 39.104026, 44.572240, 51.171810, 
  56.262906, 46.269272, 38.161230, 30.618865, 21.945145, 34.390047, 59.656971, 
  69.839163, 73.233228, 63.239594, 45.892154, 43.252326, 28.356155), ncol=2)

# fit model
m <- mahal(logo, pts)

# make a prediction
predict(m, logo[1])

x <- predict(m, logo)

# or x <- predict(logo, m) via raster::predict

# plot(x > 0)
}

\keyword{spatial}

