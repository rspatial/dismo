\name{predict}

\docType{methods}

\alias{predict}
\alias{predict,Bioclim-method}
\alias{predict,Domain-method}
\alias{predict,Mahalanobis-method}
\alias{predict,MaxEnt-method}
\alias{predict,MaxEntReplicates-method}
\alias{predict,ConvexHull-method}
\alias{predict,CircleHull-method}
\alias{predict,RectangularHull-method}
\alias{predict,CirclesRange-method}
\alias{predict,GeographicDistance-method}
\alias{predict,InvDistWeightModel-method}
\alias{predict,VoronoiHull-method}

\title{Distribution model predictions}

\description{
Make a RasterLayer with a prediction based on a model object of class the inherits from 'DistModel', including: Bioclim, Domain, MaxEnt, Mahalanobis, and GeographicDistance. Predictions with model objects that do not inherit from DistModel can be made using the similar \code{\link[raster]{predict}} function in the 'raster' package. 

Provide a Raster* object with the independent variables. The names of the layers in the Raster* object should include those expected by the model. 
}

\section{Methods}{
\describe{
\code{predict(object, x, ext=NULL, filename='', progress='text', ...)}
  
\tabular{rll}{  
  \tab  \code{object}  \tab A fitted model of class Bioclim, Domain, MaxEnt, ConvexHull, or Mahalanobis (classes that inherit from DistModel)\cr
  \tab  \code{x} \tab A Raster* object or a data.frame\cr
  \tab  \code{ext} \tab An extent object to limit the prediction to a sub-region of 'x'. Or an object that can be coerced to an Extent object by extent; such as a Raster* or Spatial* object \cr
  \tab  \code{filename}  \tab Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot \cr
  \tab \code{progress} \tab Character. Valid values are "" (no progress bar), "text" and "windows" (on that platform only) \cr
  \tab  \code{...}  \tab Additional model specific arguments. And additional arguments for file writing as for \link[raster]{writeRaster} \cr
}


For \link[dismo]{maxent} models, there is an additional argument \code{'args'} used to pass arguments (options) to the maxent software. See the help page for \code{ \link[dismo]{maxent}} for more information.
 
For \link[dismo]{bioclim} models, there is an additional argument \code{'tails'} which you can use to ignore the left or right tail of the percentile distribution for a variable. If supplied, tails should be a character vector with a length equal to the number of variables used in the model. Valid values are "both" (the default), "low" and "high". For example, if you have a variable x with an observed distribution between 10 and 20 and you are predicting the bioclim value for a value 25, the default result would be zero (outside of all observed values); but if you use tail='low', the high (right) tail is ignored and the value returned will be 1.


For \link{geoDist} models, there is an additional argument \code{fun} that allows you to use your own (inverse) distance function, and argument \code{scale=1} that allows you to scale the values (distances smaller than this value become one, and the others are divided by this value before computing the inverse distance).
}}

\value{
A RasterLayer or, (if \code{x} is a matrix), a vector. 
}

\seealso{
For spatial predictions with GLM, GAM, BRT, randomForest, etc., see \link[raster]{predict} in the Raster package.

To fit a model that can be used with this predict method, see \code{ \link{maxent}, \link{bioclim}, \link{mahal}, \link{domain}, \link{geoDist}, \link{convHull}}

Extent object: \code{\link[raster]{extent}}
}

\author{ Robert J. Hijmans}

\examples{
logo <- stack(system.file("external/rlogo.grd", package="raster"))
pts <- matrix(c(48, 48, 48, 53, 50, 46, 54, 70, 84, 85, 74, 84, 95, 85, 66, 
   42, 26, 4, 19, 17, 7, 14, 26, 29, 39, 45, 51, 56, 46, 38, 31, 22, 34, 60,
   70, 73, 63, 46, 43, 28), ncol=2)
b <- bioclim(logo, pts)
# prediction for a sub-region
e <- extent(30,90,20,60)
p <- predict(b, logo, progress='text', ext=e)
plot(p)
}

\keyword{methods}
\keyword{spatial}
