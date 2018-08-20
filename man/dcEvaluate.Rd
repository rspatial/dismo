\name{dcEvaluate}

\alias{dcEvaluate}

\title{Evaluate by distance class}

\description{
Evaluate a model for intervals of distances to the nearest point in a reference dataset for presence data and a sample of the absence data selected to have a low spatial sorting bias (obtained with pwdSample). 
}

\usage{
dcEvaluate(p, a, reference, lonlat=TRUE, binsize=15, predp, preda, model, 
               predictors, fun=predict) 
}
 
\arguments{
  \item{p}{two column matrix (x, y) or (longitude/latitude) or SpatialPoints object, for point locations}
  \item{a}{two column matrix (x, y) or (longitude/latitude) or SpatialPoints object, for point locations}
  \item{reference}{as above for reference point locations to which distances are computed}
  \item{lonlat}{ Logical. Use \code{TRUE} if the coordinates are spherical (in degrees), and use \code{FALSE} if they are planar }
  \item{binsize}{postive integer. How many presence points in each distance bin?}
  \item{predp}{p } 
  \item{preda}{a }   
  \item{model}{m }   
  \item{predictors}{pr }   
  \item{fun}{function }   
}

\value{
list with Evaluation objects
}

\author{
Robert J. Hijmans
}

\seealso{\code{\link{pwdSample}}, \code{\link{ssb}}}


\keyword{spatial}
