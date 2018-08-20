\name{gbm.fixed}
\Rdversion{1.1}
\alias{gbm.fixed}

\title{gbm fixed }

\description{
Calculates a gradient boosting (gbm) object with a fixed number of trees.  The optimal number of trees can be identified using gbm.step or some other procedure.
Mostly used as a utility function, e.g., when being called by gbm.simplify. It takes as input a dataset and arguments selecting x and y variables, learning rate and tree complexity.
}

\usage{
gbm.fixed(data, gbm.x, gbm.y, tree.complexity = 1, site.weights = rep(1, nrow(data)),
 verbose = TRUE, learning.rate = 0.001, n.trees = 2000, bag.fraction = 0.5, 
 family = "bernoulli", keep.data = FALSE, var.monotone = rep(0, length(gbm.x)))
}

\arguments{
  \item{data}{data.frame}
  \item{gbm.x}{indices of the predictors in the input dataframe}
  \item{gbm.y}{index of the response in the input dataframe}
  \item{tree.complexity}{the tree depth - sometimes referred to as interaction depth}
  \item{site.weights}{by default set equal}
  \item{verbose}{to control reporting}
  \item{learning.rate}{controls speed of the gradient descent}
  \item{n.trees}{default number of trees}
  \item{bag.fraction}{varies random sample size for each new tree}
  \item{family}{can be any of "bernoulli", "poisson", "gaussian", or "laplace"}
  \item{keep.data}{Logical. If \code{TRUE}, original data is kept}
  \item{var.monotone}{constrain to positive (1) or negative monontone (-1)}
}

\value{
object of class gbm
}

\references{
Elith, J., J.R. Leathwick and T. Hastie, 2009. A working guide to boosted regression trees. Journal of Animal Ecology 77: 802-81
}

\author{
John R. Leathwick and Jane Elith
}

\keyword{ spatial }

