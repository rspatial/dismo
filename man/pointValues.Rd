\name{pointValues}
\alias{pointValues}

\title{
point values
}


\description{
Extract values from a Raster* object for point locations. This function adds a few options that can be useful in the context of species distribution modeling to \code{\link[raster]{extract}} function in the raster package.  
}


\usage{
pointValues(x, p, a, uniquecells = TRUE, na.rm = TRUE)
}
 
\arguments{
  \item{x}{A Raster* object}
  \item{p}{Points. Two-colum matrix or data.frame; or a SpatialPoints* object}
  \item{a}{Additional points (absences). Two-colum matrix or data.frame; or a SpatialPoints* object}
  \item{uniquecells}{Logical. If \code{TRUE}, each cell can be included only once (i.e. 'duplicate' points are removed) }
  \item{na.rm}{Logical. If \code{TRUE}, cell values of NA are not returned}
}

\value{
matrix
}


\seealso{\code{\link[raster]{extract}}}

\author{
Robert J. Hijmans
}


\keyword{spatial}
