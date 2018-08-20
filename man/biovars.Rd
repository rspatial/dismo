\name{biovars}

\docType{methods}

\alias{biovars}
\alias{biovars,matrix,matrix,matrix-method}
\alias{biovars,Raster,Raster,Raster-method}
\alias{biovars,vector,vector,vector-method}

\title{bioclimatic variables}

\description{
Function to create 'bioclimatic variables' from monthly climate data. 
}

\usage{
biovars(prec, tmin, tmax, ...) 
}

\arguments{
  \item{prec}{vector, matrix, or RasterStack/Brick of precipitation data}
  \item{tmin}{vector, matrix, or RasterStack/Brick of minimum temperature data}
  \item{tmax}{vector, matrix, or RasterStack/Brick of maximum temperature data}
  \item{...}{ Additional arguments }
}

\details{
Input data is normaly monthly. I.e. there should be 12 values (layers) for each variable, but the function should also work for e.g. weekly data (with some changes in the meaning of the output variables. E.g. #8 would then not be for a quater (3 months), but for a 3 week period). 
}

\value{
Depending on the class of the input data, an object of class 'vector', 'matrix' or 'RasterBrick' with 19 variables (columns, layers)

bio1 = Mean annual temperature

bio2 = Mean diurnal range (mean of max temp - min temp)

bio3 = Isothermality (bio2/bio7) (* 100)

bio4 = Temperature seasonality (standard deviation *100)

bio5 = Max temperature of warmest month

bio6 = Min temperature of coldest month

bio7 = Temperature annual range (bio5-bio6)

bio8 = Mean temperature of the wettest quarter

bio9 = Mean temperature of driest quarter 

bio10 = Mean temperature of warmest quarter

bio11 = Mean temperature of coldest quarter

bio12 = Total (annual) precipitation

bio13 = Precipitation of wettest month

bio14 = Precipitation of driest month

bio15 = Precipitation seasonality (coefficient of variation)

bio16 = Precipitation of wettest quarter

bio17 = Precipitation of driest quarter

bio18 = Precipitation of warmest quarter
}

\author{
Robert J. Hijmans
}

\examples{
tmin <- c(10,12,14,16,18,20,22,21,19,17,15,12)
tmax <- tmin + 5
prec <- c(0,2,10,30,80,160,80,20,40,60,20,0)
biovars(prec, tmin, tmax)

tmn = tmx = prc = brick(nrow=1, ncol=1)
tmn <- setValues(tmn, t(matrix(c(10,12,14,16,18,20,22,21,19,17,15,12))))
tmx <- tmn + 5
prc <- setValues(prc, t(matrix(c(0,2,10,30,80,160,80,20,40,60,20,0))))
b <- biovars(prc, tmn, tmx)
as.matrix(b)
}


\keyword{spatial}

