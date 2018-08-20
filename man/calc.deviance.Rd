\name{calc.deviance}

\Rdversion{1.1}

\alias{calc.deviance}

\title{Calculate deviance}

\description{
Function to calculate deviance given two vectors of observed and predicted values.
Requires a family argument which is set to binomial by default
}

\usage{
calc.deviance(obs, pred, weights = rep(1,length(obs)), 
                family="binomial", calc.mean = TRUE)
}

\arguments{
  \item{obs}{a vector with observed values}
  \item{pred}{a vector with predicted values that correspond the the values in obs}
  \item{weights}{a vector of weight values}
  \item{family}{One of "binomial", "bernoulli", "poisson", "laplace", or "gaussian"}
  \item{calc.mean}{Logical. If \code{TRUE}, the mean deviance is returned}
}


\author{
John R. Leathwick and Jane Elith
}

\keyword{ spatial }

