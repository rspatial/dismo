\name{gbm.interactions}

\Rdversion{1.1}

\alias{gbm.interactions}

\title{gbm interactions}

\description{
Tests whether interactions have been detected and modelled, and reports the relative strength of these. 
Results can be visualised with gbm.perspec

The function assesses the magnitude of 2nd order interaction effects in gbm models fitted with interaction depths greater than 1. This is achieved by:

  1. forming predictions on the linear scale for each predictor pair;
  
  2. fitting a linear model that relates these predictions to the predictor pair, with the the predictors fitted as factors;
  
  3. calculating the mean value of the residuals, the magnitude of which increases with the strength of any interaction effect;
  
  4. results are stored in an array;
  
  5. finally, the n most important interactions are identified, where n is 25% of the number of interaction pairs;
}

\usage{
gbm.interactions(gbm.object, use.weights=FALSE, mask.object)
}

\arguments{
  \item{gbm.object}{A gbm object}
  \item{use.weights}{Logical. If \code{TRUE}, weights are used for samples }
  \item{mask.object}{a gbm object describing sample intensity}
}


\value{
object of class gbm
}

\keyword{ spatial }

