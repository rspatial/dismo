\name{gbm.simplify}

\Rdversion{1.1}

\alias{gbm.simplify}

\title{gbm simplify}

\description{
The function takes an inital cross-validated model as produced by gbm.step 
and then assesses the potential to remove predictors using k-fold cross validation. 
This done for each fold, removing the lowest contributing predictor, 
and repeating this process for a set number of steps. 
After the removal of each predictor, the change in predictive deviance 
is computed relative to that obtained when using all predictors. 
The function returns a list containing the mean change in deviance and its standard error 
as a function of the number of variables removed. 
Having completed the cross validation, it then identifies the sequence 
of variable to remove when using the full data set, testing this 
up to the number of steps used in the cross-validation phase of the analysis 
with results reported to the screen. 

The function returns a table containing the order in which variables are to be removed 
and some vectors, each of which specifies the predictor column numbers 
in the original dataframe  - the latter can be used as an argument to gbm.step
e.g., gbm.step(data = data, gbm.x = simplify.object$pred.list[[4]]...
would implement a new analysis with the original predictor set, minus its
four lowest contributing predictors.
}

\usage{
gbm.simplify(gbm.object, n.folds = 10, n.drops = "auto", alpha = 1, prev.stratify = TRUE, 
   eval.data = NULL, plot = TRUE)
}


\arguments{
  \item{gbm.object}{a gbm object describing sample intensity }
  \item{n.folds}{number of times to repeat the analysis}
  \item{n.drops}{can be automatic or an integer specifying the number of drops to check}
  \item{alpha}{controls stopping when n.drops = "auto"}
  \item{prev.stratify}{use prevalence stratification in selecting evaluation data}
  \item{eval.data}{an independent evaluation data set - leave here for now}
  \item{plot}{plot results}
}
\value{
A list with these elements: 
deviance.summary, deviance.matrix, drop.count, final.drops, pred.list, and gbm.call = gbm.call))
}


\references{
Elith, J., J.R. Leathwick and T. Hastie, 2009. A working guide to boosted regression trees. Journal of Animal Ecology 77: 802-81
}


\author{
John R. Leathwick and Jane Elith
}

\keyword{ spatial }

