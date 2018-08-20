\name{Random null model}

\alias{nullRandom}


\title{Random null model}


\description{
Null model based on randomization of locations as suggested by Raes and ter Steege (2007).
}

\usage{
nullRandom(x, model, n=25, rep=25, pa=FALSE)
}
 
\arguments{
  \item{x}{data.frame with environmental predictor values for collecting localities } 
  \item{model}{Model function that creates a model of class 'DistModel' }
  \item{n}{Sample size }
  \item{rep}{Number of repetitions }
  \item{pa}{ Boolean. Prensence-only or presence/background model (e.g. Maxent) }
}


\value{
List with \code{n} object of class \code{\link{ModelEvaluation-class}}
}

\author{
Robert J. Hijmans
}

\references{
Raes, N. & H. ter Steege, 2007. A null-model for significance testing of presence-only species distribution models. Ecography 30:727-736.
}

\seealso{
\code{\link[dismo]{geoDist}}
}

\examples{
predictors <- stack(list.files(path=paste(system.file(package="dismo"), '/ex', sep=''), 
               pattern='grd', full.names=TRUE ))
occurence <- paste(system.file(package="dismo"), '/ex/bradypus.csv', sep='')
occ <- read.table(occurence, header=TRUE, sep=',')[,-1]
 
x <- extract(predictors, occ)
nr <- nullRandom(x, bioclim, n=25, rep=25, pa=FALSE)
mean(sapply(nr, function(x)x@auc))
}


\keyword{spatial}

