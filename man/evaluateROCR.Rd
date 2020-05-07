\name{evaluateROCR}

\alias{evaluateROCR}

\title{
Model testing with the ROCR package
}

\description{
Preparing data for model testing with the ROCR package.
}

\usage{
evaluateROCR(model, p, a, x)
}
 
\arguments{
  \item{model}{any fitted model, including objects inheriting from 'DistModel'}
  
  \item{p}{presence points (x and y coordinates or SpatialPoints* object). 
  
           Or, if \code{x} is missing, values at presence points
		   
		   Or, a matrix with values to compute predictions for}
		   
  \item{a}{absence points (x and y coordinates or SpatialPoints* object). 
			
			Or, if \code{x} is missing, values at presence points.

			Or, a matrix with values to compute predictions for}
			
  \item{x}{optional. predictor variables, if present, \code{p} and \code{a} are considered }
}

\value{
An object of class "prediction" (defined in the ROCR package)
}

\author{
Robert J. Hijmans
}

\keyword{spatial}

