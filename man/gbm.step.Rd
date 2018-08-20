\name{gbm.step}

\Rdversion{1.1}

\alias{gbm.step}

\title{
gbm step
}

\description{
Function to assess the optimal number of boosting trees using k-fold cross validation. This is an implementation of the cross-validation procedure described on page 215 of Hastie et al (2001).

The data is divided into 10 subsets, with stratification by prevalence if required for presence/absence data. The function then fits a gbm model of increasing complexity along the sequence from \code{n.trees} to \code{n.trees + (n.steps * step.size)}, calculating the residual deviance at each step along the way. After each fold processed, the function calculates the average holdout residual deviance and its standard error and then identifies the optimal number of trees as that at which the holdout deviance is minimised. It fits a model with this number of trees, returning it as a gbm model along with additional information from the cross-validation selection process.
}

\usage{
gbm.step(data, gbm.x, gbm.y, offset = NULL, fold.vector = NULL, tree.complexity = 1,
 learning.rate = 0.01, bag.fraction = 0.75, site.weights = rep(1, nrow(data)), 
 var.monotone = rep(0, length(gbm.x)), n.folds = 10, prev.stratify = TRUE, 
 family = "bernoulli", n.trees = 50, step.size = n.trees, max.trees = 10000,
 tolerance.method = "auto", tolerance = 0.001, plot.main = TRUE, plot.folds = FALSE,
 verbose = TRUE, silent = FALSE, keep.fold.models = FALSE, keep.fold.vector = FALSE, 
 keep.fold.fit = FALSE, ...)
}

\arguments{
  \item{data}{input data.frame}
  \item{gbm.x}{indices or names of predictor variables in \code{data}}
  \item{gbm.y}{index or name of response variable in \code{data}}
  \item{offset}{offset}
  \item{fold.vector}{a fold vector to be read in for cross validation with offsets}
  \item{tree.complexity}{sets the complexity of individual trees}
  \item{learning.rate}{sets the weight applied to inidivudal trees}
  \item{bag.fraction}{sets the proportion of observations used in selecting variables}
  \item{site.weights}{allows varying weighting for sites}
  \item{var.monotone}{restricts responses to individual predictors to monotone}
  \item{n.folds}{number of folds}
  \item{prev.stratify}{prevalence stratify the folds - only for presence/absence data}
  \item{family}{family - bernoulli (=binomial), poisson, laplace or gaussian}
  \item{n.trees}{number of initial trees to fit}
  \item{step.size}{numbers of trees to add at each cycle}
  \item{max.trees}{max number of trees to fit before stopping}
  \item{tolerance.method}{method to use in deciding to stop - "fixed" or "auto"}
  \item{tolerance}{tolerance value to use - if method == fixed is absolute, if auto is multiplier * total mean deviance}
  \item{plot.main}{Logical. plot hold-out deviance curve}
  \item{plot.folds}{Logical. plot the individual folds as well}
  \item{verbose}{Logical. control amount of screen reporting}
  \item{silent}{Logical. to allow running with no output for simplifying model)}
  \item{keep.fold.models}{Logical. keep the fold models from cross valiation}
  \item{keep.fold.vector}{Logical. allows the vector defining fold membership to be kept}
  \item{keep.fold.fit}{Logical. allows the predicted values for observations from cross-validation to be kept}
  \item{\dots}{Logical. allows for any additional plotting parameters}
}

\note{
This and other boosted regression trees (BRT) functions in the dismo package do not work if you use only one predictor. There is an easy work around: make a dummy variable with a constant value and then fit a model with two predictors, the one of interest and the dummy variable, which will be ignored by the model fitting as it has no useful information.
}

\value{
object of S3 class gbm
}

\references{
Hastie, T., R. Tibshirani, and J.H. Friedman, 2001. The Elements of Statistical Learning: Data Mining, Inference, and Prediction. Springer-Verlag, New York
Elith, J., J.R. Leathwick and T. Hastie, 2009. A working guide to boosted regression trees. Journal of Animal Ecology 77: 802-81
}

\author{
John R. Leathwick and Jane Elith
}

\examples{
data(Anguilla_train)
# reduce data set to speed things up a bit
Anguilla_train = Anguilla_train[1:200,]
angaus.tc5.lr01 <- gbm.step(data=Anguilla_train, gbm.x = 3:14, gbm.y = 2, family = "bernoulli",
       tree.complexity = 5, learning.rate = 0.01, bag.fraction = 0.5)
}

\keyword{ spatial }
