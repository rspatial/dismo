\name{density}

\alias{density}
\alias{density,DistModel-method}
\alias{density,ModelEvaluation-method}

\docType{methods}

\title{density}

\description{
Create a density plots of presence and absence data
}


\section{Methods}{
\describe{
\code{density(x, ...)}
  
\tabular{rll}{  
 \tab  \code{x}  \tab Object of class 'ModelEvaluation' or of a class that inherits from 'DistModel, (such as 'MaxEnt', 'Bioclim') \cr
 \tab  \code{...}  \tab Additional arguments that can be passed to plot\cr
}
}
}

\value{
A density plot. Presence data are in red, and absence data (if available) are in blue.
}

\seealso{
\code{\link[dismo]{evaluate} }
}

\author{Robert J. Hijmans}


\keyword{spatial}
