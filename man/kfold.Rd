\name{kfold}

\alias{kfold}

\title{
k-fold partitioning
}

\description{
k-fold partitioning of a data set for model testing purposes. Each record in a matrix (or similar data structure) is randomly assigned to a group. Group numbers are between 1 and \code{k}. 
}


\usage{
kfold(x, k=5, by)
}
 
\arguments{
  \item{x}{a vector, matrix, data.frame, or Spatial object}
  \item{k}{number of groups}
  \item{by}{Optional argument. A vector or factor with sub-groups (e.g. species). Its length should be the same as the number of records in x}
}

\value{
a vector with group assignments
}

\author{
Robert J. Hijmans
}

\examples{

#library(disdat)
#data(NSWtrain)
## a single species
#srsp1 <- subset(NSWtrain, spid=='srsp1')
#kfold(srsp1, k = 5)

## all species
#k = kfold(NSWtrain, k=5, by=NSWtrain$spid)

#k[NSWtrain$spid=='srsp1']
## each group has the same number of records 
##(except for adjustments if the number of records divided by k is not an integer) 
#table(k[NSWtrain$spid=='srsp1'])
#k[NSWtrain$spid=='ousp5']

}

\keyword{spatial}

