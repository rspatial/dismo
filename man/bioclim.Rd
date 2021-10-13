\name{bioclim}

\alias{bioclim}
\alias{bioclim,Raster,matrix-method}
\alias{bioclim,Raster,SpatialPoints-method}
\alias{bioclim,Raster,data.frame-method}
\alias{bioclim,SpatialGridDataFrame,matrix-method}
\alias{bioclim,SpatialGridDataFrame,SpatialPoints-method}
\alias{bioclim,matrix,missing-method}
\alias{bioclim,data.frame,missing-method}
\alias{Bioclim-class}


\title{Bioclim}


\description{
The Bioclim algorithm has been extensively used for species distribution modeling. Bioclim is the classic 'climate-envelope-model'. Although it generally does not perform as good as some other modeling methods (Elith et al. 2006) and is unsuited for predicting climate change effects (Hijmans and Graham, 2006). It is still used, however, among other reasons because the algorithm is easy to understand and thus useful in teaching species distribution modeling. 

The BIOCLIM algorithm computes the similarity of a location by comparing the values of environmental variables at any location to a percentile distribution of the values at known locations of occurrence ('training sites'). The closer to the 50th percentile (the median), the more suitable the location is. The tails of the distribution are not distinguished, that is, 10 percentile is treated as equivalent to 90 percentile. 

In this R implementation, percentile scores are between 0 and 1, but predicted values larger than 0.5 are subtracted from 1. Then, the minimum percentile score across all the environmental variables is computed (i.e. this is like Liebig's law of the minimum, except that high values can also be limiting factors). The final value is subtracted from 1 and multiplied with 2 so that the results are between 0 and 1. The reason for this transformation is that the results become more like that of other distribution modeling methods and are thus easier to interpret. The value 1 will rarely be observed as it would require a location that has the median value of the training data for all the variables considered. The value 0 is very common as it is assigned to all cells with a value of an environmental variable that is outside the percentile distribution (the range of the training data) for at least one of the variables. 

In the \link[dismo]{predict} function, you can choose to ignore one of the tails of the distribution (e.g, to make low rainfall a limiting factor, but not high rainfall),
}


\usage{
bioclim(x, p, ...)
}
 

\arguments{
  \item{x}{ Raster* object or matrix}
  \item{p}{ two column matrix or SpatialPoints* object}
  \item{...}{ Additional arguments }
}


\value{
An object of class 'Bioclim' (inherits from \code{\link{DistModel-class}})
}


\references{
Nix, H.A., 1986. A biogeographic analysis of Australian elapid snakes. In: Atlas of Elapid Snakes of Australia. (Ed.) R. Longmore, pp. 4-15. Australian Flora and Fauna Series Number 7. Australian Government Publishing Service: Canberra.

Booth, T.H., H.A. Nix, J.R. Busby and M.F. Hutchinson, 2014. BIOCLIM: the first species distribution modelling package, its early applications and relevance to most current MAXENT studies. Diversity and Distributions 20: 1-9

Elith, J., C.H. Graham, R.P. Anderson, M. Dudik, S. Ferrier, A. Guisan, R.J. Hijmans, F. Huettmann, J. Leathwick, A. Lehmann, J. Li, L.G. Lohmann, B. Loiselle, 
G. Manion, C. Moritz, M. Nakamura, Y. Nakazawa, J. McC. Overton, A.T. Peterson, S. Phillips, K. Richardson, R. Scachetti-Pereira, R. Schapire, J. Soberon, 
S. Williams, M. Wisz and N. Zimmerman, 2006. 
Novel methods improve prediction of species' distributions from occurrence data. Ecography 29: 129-151. \doi{10.1111/j.2006.0906-7590.04596.x}

Hijmans R.J., and C.H. Graham, 2006. Testing the ability of climate envelope models to predict the effect of climate change on species distributions. Global change biology 12: 2272-2281. \doi{10.1111/j.1365-2486.2006.01256.x}
}



\author{
Robert J. Hijmans
}

\seealso{
\code{\link[dismo]{predict}, \link[dismo]{maxent}, \link[dismo]{domain}, \link[dismo]{mahal}}
}


\examples{
logo <- stack(system.file("external/rlogo.grd", package="raster"))
#presence data
pts <- matrix(c(48.243420, 48.243420, 47.985820, 52.880230, 49.531423, 46.182616, 54.168232, 
  69.624263, 83.792291, 85.337894, 74.261072, 83.792291, 95.126713, 84.565092, 66.275456, 41.803408,
  25.832176, 3.936132, 18.876962, 17.331359,7.048974, 13.648543, 26.093446, 28.544714, 39.104026, 
  44.572240, 51.171810, 56.262906, 46.269272, 38.161230, 30.618865, 21.945145, 34.390047, 59.656971,
  69.839163, 73.233228, 63.239594, 45.892154, 43.252326, 28.356155) , ncol=2)
bc <- bioclim(logo, pts)

#or
v <- extract(logo, pts)
bc <- bioclim(v)
p1 <- predict(logo, bc)
p2 <- predict(logo, bc, tails=c('both', 'low', 'high'))

#or
#sp <- SpatialPoints(pts)
#bc <- bioclim(logo, pts)
}

\keyword{spatial}

