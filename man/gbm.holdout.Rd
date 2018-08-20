\name{gbm.holdout}
\Rdversion{1.1}
\alias{gbm.holdout}

\title{gbm holdout}

\description{
Calculates a gradient boosting (gbm) object in which model complexity is 
determined using a training set with predictions made to a withheld set.
An initial set of trees is fitted, and then trees are progressively added
testing performance along the way, using gbm.perf until the optimal
number of trees is identified.

As any structured ordering of the data should be avoided, a copy of the data set is 
BY DEFAULT randomly reordered each time the function is run.
}

\usage{
gbm.holdout(data, gbm.x, gbm.y, learning.rate = 0.001, tree.complexity = 1, 
 family = "bernoulli", n.trees = 200, add.trees = n.trees, max.trees = 20000, 
 verbose = TRUE, train.fraction = 0.8, permute = TRUE, prev.stratify = TRUE,
 var.monotone = rep(0, length(gbm.x)), site.weights = rep(1, nrow(data)), 
 refit = TRUE, keep.data = TRUE)
}

\arguments{
  \item{data}{data.frame}
  \item{gbm.x}{indices of the predictors in the input dataframe}
  \item{gbm.y}{index of the response in the input dataframe}
  \item{learning.rate}{typically varied between 0.1 and 0.001}
  \item{tree.complexity}{sometimes called interaction depth}
  \item{family}{"bernoulli","poisson", etc. as for gbm}
  \item{n.trees}{initial number of trees}
  \item{add.trees}{number of trees to add at each increment}
  \item{max.trees}{maximum number of trees to fit}
  \item{verbose}{controls degree of screen reporting}
  \item{train.fraction}{proportion of data to use for training}
  \item{permute}{reorder data to start with}
  \item{prev.stratify}{stratify selection for presence/absence data}
  \item{var.monotone}{allows constraining of response to monotone }
  \item{site.weights}{set equal to 1 by default}
  \item{refit}{refit the model with the full data but id'd no of trees}
  \item{keep.data}{keep copy of the data}
}


\value{
A gbm object
}

\references{
Elith, J., J.R. Leathwick and T. Hastie, 2009. A working guide to boosted regression trees. Journal of Animal Ecology 77: 802-81
}

\author{
John R. Leathwick and Jane Elith
}


\keyword{ spatial }

