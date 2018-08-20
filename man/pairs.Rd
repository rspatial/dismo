\name{pairs}

\docType{methods}

\alias{pairs}
\alias{pairs,DistModel-method}

\title{
Pair plots
}

\description{
Pair plots of presence and absence (background) data.
}

\section{Methods}{
\describe{
\code{pairs(x, v=NULL, pa='pa', hist=TRUE, cor=TRUE)}
  
\tabular{rll}{  
 \tab  \code{x}  \tab Object of class DistModel or derived from that class (such as 'MaxEnt', 'Bioclim') \cr
\tab \code{v} \tab numeric, to select a subset of pairs, e.g. v=1:3 to plot only the first three variables \cr
\tab \code{pa} \tab Character. Either 'pa', 'p', or 'a' to show presence and absence, presence, or absence data respectively\cr
\tab \code{hist} \tab logical. If \code{TRUE} a histogram of the values is shown on the diagonal\cr
\tab \code{cor} \tab logical. If \code{TRUE} the correlation coefficient is shown in the upper panels \cr
}		
}
}

\seealso{
\code{\link[dismo]{density}, \link[dismo]{plot}}
}

\author{
Robert J. Hijmans
}

\keyword{spatial}

