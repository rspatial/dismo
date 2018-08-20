\name{prepareData}

\alias{prepareData}

\title{
Prepare data for model fitting
}

\description{
Simple helper function to prepare data for model fitting
}

\usage{
prepareData(x, p, b, factors, xy=FALSE) 
}

\arguments{
\item{x}{Raster* object}
\item{p}{presence points}
\item{b}{background (or absence) points}
\item{factors}{vectors indicating which variables are factors (using layer names or numbers)}
\item{xy}{logical. If \code{TRUE}, the first two columns of the returned data.frame will be the coordinates of p and b (labeled 'x' and 'y')}
}

\value{
data.frame with \code{nlayers(x)+1} columns and \code{nrow(p) + nrow(b)} rows. The first column, 'pb' indicates whether a record represents presence '1' or background '0' values. The other columns have the values from the Raster* object.
}

\author{
Robert J. Hijmans
}

\keyword{spatial}

