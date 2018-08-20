\name{threshold}

\alias{threshold}
\alias{threshold,ModelEvaluation-method}


\title{ Find a threshold }

\description{ 
Find a threshold (cut-off) to transform model predictions (probabilities, distances, or similar values) to a binary score (presence or absence). 
}

\usage{
\S4method{threshold}{ModelEvaluation}(x, stat='', sensitivity=0.9, ...)
}
 
\arguments{
  \item{x}{A ModelEvaluation object (see \code{\link{evaluate}}}
  \item{stat}{character. To select a particular threshold (see section 'value' for possible values)} 
  \item{sensitivity}{numeric between 0 and 1. For the fixed sensitivity threshold}
  \item{...}{Additional arguments. None implemented} 
}

\value{
data.frame with the following columns:

kappa: the threshold at which kappa is highest ("max kappa")

spec_sens: the threshold at which the sum of the sensitivity (true positive rate) and specificity (true negative rate) is highest

no_omission: the highest threshold at which there is no omission 

prevalence: modeled prevalence is closest to observed prevalence
		
equal_sens_spec: equal sensitivity and specificity

sensitivty: fixed (specified) sensitivity
}

\author{
Robert J. Hijmans and Diego Nieto-Lugilde
}


\seealso{\code{\link{evaluate}}}

\examples{
## See ?maxent for an example with real data.
# this is a contrived example:
# p has the predicted values for 50 known cases (locations)
# with presence of the phenomenon (species)
p <- rnorm(50, mean=0.7, sd=0.3)
# b has the predicted values for 50 background locations (or absence)
a <- rnorm(50, mean=0.4, sd=0.4)
e <- evaluate(p=p, a=a)

threshold(e)
}

\keyword{spatial}

