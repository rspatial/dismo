\name{Evaluation plots}

\docType{methods}

\alias{plot,ModelEvaluation,character-method}


\title{
Plot model evaluation data 
}

\description{
Make a ROC curve, or a plot of a threshold dependent measure against threshold values
}

\section{Methods}{
usage:  \code{plot(x, y, ...)}
\tabular{rll}{
\tab \code{x}  \tab  Object of class ModelEvaluation  \cr
\tab \code{y}  \tab  Character. Either 'ROC' or a threshold dependent measure such as 'kappa', 'TPR' \cr
\tab \code{...}  \tab  Additional arguments that can be passed to \link[base]{plot} \cr
}
}

\seealso{
\code{\link{ModelEvaluation-class}, \link[dismo]{density}, \link[dismo]{pairs}, \link[base]{plot} }
}

\author{
Robert J. Hijmans
}


\examples{
# p = the predicted value for 50 known cases (locations) with presence of the phenomenon (species)
p = rnorm(50, mean=0.7, sd=0.3)
# b = the predicted value for 50 known cases (locations) with absence of the phenomenon (species)
a = rnorm(50, mean=0.4, sd=0.4)
e = evaluate(p=p, a=a)
plot(e, 'ROC')
plot(e, 'kappa')
plot(e, 'FPR')
plot(e, 'prevalence')
}

\keyword{spatial}

