\name{ecocrop}

\alias{ecocrop}
\alias{getCrop}
\alias{ECOcrops}
\alias{ECOCROP-class}
\alias{ECOCROPcrop-class}


\title{Ecocrop model}

\description{
Very simple mechanistic model for plants. 
}

\usage{
ecocrop(crop, tmin, tavg, prec, rainfed=TRUE, ...) 
getCrop(name)
data(ECOcrops)
}
 

\arguments{
  \item{crop}{An object of class 'ECOCROP', or the name of a crop as in getCrop}
  \item{tmin}{Vector of monthly minimum temperature (degrees C)}
  \item{tavg}{Vector of monthly average temperature (degrees C)}
  \item{prec}{Vector of monthly precipitation (mm)}
  \item{rainfed}{Logical. If \code{FALSE}, the crop is assumed to be irrigated}
  \item{...}{Additinal arguments}
  \item{name}{Name of a crop (character). If missing a data.frame with all crop names is returned}
}


\value{
Object of class ECOCROP
}

\author{
Robert J. Hijmans
}

\examples{
ecocrop('potato', 5:16, 15:26, runif(12)*100)
getCrop('Acacia brachystachya Benth.')
crop <- getCrop('Hot pepper')
ecocrop(crop, 5:16, 15:26, rainfed=FALSE)
}

\keyword{spatial}

