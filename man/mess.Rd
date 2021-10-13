\name{mess}

\alias{mess}

\title{Multivariate environmental similarity surfaces (MESS)}

\description{Compute multivariate environmental similarity surfaces (MESS), as described by Elith et al., 2010}

\usage{mess(x, v, full=FALSE, filename='', ...)}

\arguments{
  \item{x}{Raster* object}
  \item{v}{matrix or data.frame containing the reference values. Each column should correspond to one layer of the Raster* object}
  \item{full}{logical. If \code{FALSE} a RasterLayer with the MESS values is returned. If \code{TRUE}, a RasterBrick is returned with \code{n} layers corresponding to the layers of the input Raster object and an additional layer with the MESS values}
  \item{filename}{character. Output filename (optional)}
  \item{...}{additional arguments as for \code{\link{writeRaster}}}
}

\details{
\code{v} can be obtained for a set of points using \code{\link[raster]{extract}} .
}

\value{
A RasterBrick with layers corresponding to the input layers and an additional layer with the mess values (if \code{full=TRUE} and \code{nlayers(x) > 1}) or a RasterLayer with the MESS values (if \code{full=FALSE}).
}

\references{
Elith J., M. Kearney M., and S. Phillips, 2010. The art of modelling range-shifting species. \doi{10.1111/j.2041-210X.2010.00036.x}{Methods in Ecology and Evolution 1:330-342}.
}

\author{
Jean-Pierre Rossi <jean-pierre.rossi@supagro.inra.fr>, Robert Hijmans, Paulo van Breugel
}


\examples{

set.seed(9)
r <- raster(ncol=10, nrow=10)
r1 <- setValues(r, (1:ncell(r))/10 + rnorm(ncell(r)))
r2 <- setValues(r, (1:ncell(r))/10 + rnorm(ncell(r)))
r3 <- setValues(r, (1:ncell(r))/10 + rnorm(ncell(r)))
s <- stack(r1,r2,r3)
names(s) <- c('a', 'b', 'c')
xy <- cbind(rep(c(10,30,50), 3), rep(c(10,30,50), each=3))
refpt <- extract(s, xy)

ms <- mess(s, refpt, full=TRUE)
plot(ms)


\dontrun{
filename <- paste(system.file(package="dismo"), '/ex/bradypus.csv', sep='')
bradypus <- read.table(filename, header=TRUE, sep=',')
bradypus <- bradypus[,2:3]
files <- list.files(path=paste(system.file(package="dismo"),'/ex', sep=''), 
   pattern='grd', full.names=TRUE )
predictors <- stack(files)
predictors <- dropLayer(x=predictors,i=9)
reference_points <- extract(predictors, bradypus)
mss <- mess(x=predictors, v=reference_points, full=TRUE)
plot(mss)
}

}

