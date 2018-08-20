\name{domain}
\alias{domain}
\alias{domain,Raster,SpatialPoints-method}
\alias{domain,Raster,matrix-method}
\alias{domain,Raster,data.frame-method}
\alias{domain,matrix,missing-method}
\alias{domain,data.frame,missing-method}
\alias{Domain-class}


\title{
Domain
}

\description{
The Domain algorithm (Carpenter et al. 1993) that has been extensively used for species distribution modeling. It is included here for that reason but please note that it generally does not perform very well in model comparison (Elith et al. 2006, Hijmans and Graham, 2006). The Domain algorithm computes the Gower distance between environmental variables at any location and those at any of the known locations of occurrence ('training sites'). For each variable the minimum distance between a site and any of the training points is taken. To integrate over environmental variables, the maximum distance to any of the variables is used. This distance is subtracted from one, and (in this R implementation) values below zero are truncated so that the scores are between 0 (low) and 1 (high).
}

\usage{
domain(x, p, ...)
}
 
\arguments{
  \item{x}{ Raster* object or matrix}
  \item{p}{ two column matrix or SpatialPoints* object}
  \item{...}{ Additional arguments }
}


\value{
An object of class 'Domain' (inherits from \code{\link{DistModel-class}})
}


\examples{
logo <- stack(system.file("external/rlogo.grd", package="raster"))
#presence data
pts <- matrix(c(48.243420, 48.243420, 47.985820, 52.880230, 49.531423, 46.182616, 54.168232, 
  69.624263, 83.792291, 85.337894, 74.261072, 83.792291, 95.126713, 84.565092, 66.275456, 
  41.803408, 25.832176, 3.936132, 18.876962, 17.331359,7.048974, 13.648543, 26.093446, 
  28.544714, 39.104026, 44.572240, 51.171810, 56.262906, 46.269272, 38.161230, 30.618865,
  21.945145, 34.390047, 59.656971, 69.839163, 73.233228, 63.239594, 45.892154, 43.252326,
  28.356155), ncol=2)
d <- domain(logo, pts)
p <- predict(d, logo)
}

\seealso{
\code{\link[dismo]{predict}, \link[dismo]{maxent}, \link[dismo]{bioclim}, \link[dismo]{mahal}}
}

\references{
Carpenter G., A.N. Gillison and J. Winter, 1993. Domain: a flexible modelling procedure for  mapping potential distributions of plants and animals. Biodiversity Conservation 2:667-680.

Elith, J., C.H. Graham, R.P. Anderson, M. Dudik, S. Ferrier, A. Guisan, R.J. Hijmans, F. Huettmann, J. Leathwick, A. Lehmann, J. Li, L.G. Lohmann, B. Loiselle, 
G. Manion, C. Moritz, M. Nakamura, Y. Nakazawa, J. McC. Overton, A.T. Peterson, S. Phillips, K. Richardson, R. Scachetti-Pereira, R. Schapire, J. Soberon, 
S. Williams, M. Wisz and N. Zimmerman, 2006. 
Novel methods improve prediction of species' distributions from occurrence data. Ecography 29: 129-151. \url{http://dx.doi.org/10.1111/j.2006.0906-7590.04596.x}

Hijmans R.J., and C.H. Graham, 2006. Testing the ability of climate envelope models to predict the effect of climate change on species distributions. Global change biology 12: 2272-2281. \url{http://dx.doi.org/10.1111/j.1365-2486.2006.01256.x}
}

\author{
Robert J. Hijmans
}

\keyword{spatial}

