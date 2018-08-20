\name{gridSample}

\alias{gridSample}

\title{Stratified regular sample on a grid}


\description{
Sample points from xy, using a grid (raster) as stratification. Up to n points are sampled from each stratum (cell). For "chessboard" sampling (i.e. sampling from half the cells), use the argument \code{chess='black'}, or \code{chess='white'}.
}

\usage{
gridSample(xy, r, n=1, chess='') 
}

\arguments{
  \item{xy}{A two column matrix or data.frame with x and y coordinates (or longitude and latitude), or a SpatialPoints* object}
  \item{r}{Raster* object}
  \item{n}{Maximum number of samples per cell}  
  \item{chess}{Character. '', 'black', or 'white'. If 'black' or 'white', "chess-board" sampling is used. I.e. only the 'white' fields, or only the 'black' fields are sampled. Cell number 1 (the upper left corner of \code{r}) is white.}  
}


\value{
A two column matrix with x and y coordinates (or longitude and latitude)
}

\author{Robert J. Hijmans}

\seealso{ 
\code{\link{pwdSample}}
}

\examples{
x <- rnorm(1000, 10, 5)
y <- rnorm(1000, 50, 5)
xy <- cbind(x,y)
res <- 5
r <- raster(extent(range(xy[,1]), range(xy[,2])) + res)
res(r) <- res

samp <- gridSample(xy, r, n=1)
plot(xy, cex=0.1)
points(samp, pch='x', col='red')
}

\keyword{spatial}

