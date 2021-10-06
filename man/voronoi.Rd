\name{voronoi}

\Rdversion{1.1}

\alias{voronoi}
\alias{voronoi,ANY-method}

\title{
Voronoi polygons
}

\description{
Create Voronoi polygons for a set of points. (These are also known Thiessen polygons, and Nearest Neighbor polygons; and the technique used is referred to as Delauny triangulation.)
}

\usage{
\S4method{voronoi}{ANY}(x, ext, eps=1e-09, ...)
}

\arguments{
 \item{x}{SpatialPoints* or two column matrix with x and y coordinates}
 \item{ext}{Extent. Can be used to set the corners of the rectangular window enclosing the triangulation. The default is the data range plus 10 percent. See \code{\link[deldir]{deldir}}}
 \item{eps}{Numerical tolerance used in triangulation. See \code{\link[deldir]{deldir}}}
 \item{...}{Additional arguments (none)} 
}

\value{
SpatialPolygonsDataFrame
}

\author{
This method is based on the \code{link[deldir]{deldir}} function by Rolf Turner and code by Carson Farmer
}


\examples{
# points
p <- matrix(c(17, 42, 85, 70, 19, 53, 26, 84, 84, 46, 48, 85, 4, 95, 48, 54, 66, 74, 50, 48, 
      28, 73, 38, 56, 43, 29, 63, 22, 46, 45, 7, 60, 46, 34, 14, 51, 70, 31, 39, 26), ncol=2)
	  
v <- voronoi(p)
v
}


\keyword{ spatial  }
