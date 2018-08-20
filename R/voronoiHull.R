# Author: Robert J. Hijmans
# Date : Febrary 2010, June 2016
# Version 1.0
# Licence GPL v3


setClass('VoronoiHull',
	contains = 'DistModel',
	representation (
		polygons ='SpatialPolygonsDataFrame'
	),	
	prototype (	
	),
	validity = function(object)	{
		return(TRUE)
	}
)

if (!isGeneric("voronoiHull")) {
	setGeneric("voronoiHull", function(p, a, ...)
		standardGeneric("voronoiHull"))
}	

setMethod('voronoiHull', signature(p='matrix', a='matrix'), 
	function(p, a, ext=NULL, dissolve=FALSE, crs=NA, ...) {
		v <- new('VoronoiHull')
		
		p <- stats::na.omit(unique(p))
		a <- stats::na.omit(unique(a))
		xy <- rbind(p,a)
		pa <- c(rep(1, nrow(p)), rep(0, nrow(a)))
		paxy <- cbind(pa, xy) 
		paxy[duplicated(paxy[, 2:3]), 1] <- 1  # duplicates are "present" (= "1")
		paxy <- unique(paxy)

		v@presence <- data.frame(paxy[paxy[,1]==1, -1])
		v@absence <- data.frame(paxy[paxy[,1]==0, -1])
	
		vor <- voronoi(paxy[, -1, drop=FALSE], ext=ext, dissolve=dissolve, ...)

		absence <- which(paxy[,1] == 0)
		pa <- rep(1, length(vor))
		pa[vor$id %in% absence] <- 0
		vor$pa <- pa
		vor$id <- NULL
		
		if (dissolve) { 
			vor <- aggregate(vor, 'pa')
		}
		
		crs(vor) <- crs
		v@polygons <- vor
		return(v)
	}
)


setMethod('voronoiHull', signature(p='data.frame', a='data.frame'), 
	function(p, a, ext=NULL, dissolve=FALSE, crs=NA, ...) {
		voronoiHull(as.matrix(p), as.matrix(a), ext=ext, dissolve=dissolve, crs=crs, ...)
	}
)


setMethod('voronoiHull', signature(p='SpatialPoints', a='SpatialPoints'), 
	function(p, a, ext=NULL, dissolve=FALSE, ...) {
		v <- voronoiHull(coordinates(p), coordinates(a), ext=ext, dissolve=dissolve, ...)
		if (is.null(list(...)$crs)) {
			crs(v) <- crs(p)
		}
		v
	}
)


setMethod("plot", signature(x='VoronoiHull', y='missing'), 
	function(x, ...) {
		sp <- x@polygons
		sp::plot(sp, ...)
	}
)


setMethod("polygons", "VoronoiHull",
	function(obj) {
		obj@polygons
	}
)

setMethod("geometry", "VoronoiHull",
	function(obj) {
		geometry(obj@polygons)
	}
)
