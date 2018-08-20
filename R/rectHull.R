# Author: Robert Hijmans
# Date : June 2016
# Version 1.0
# Licence GPL v3


setClass('RectangularHull',
	contains = 'DistModel',
	representation (
		polygons='SpatialPolygonsDataFrame'
	),	
	prototype (	
	),
	validity = function(object)	{
		return(TRUE)
	}
)


setMethod("polygons", "RectangularHull",
	function(obj) {
		obj@polygons
	}
)

setMethod("geometry", "RectangularHull",
	function(obj) {
		geometry(obj@polygons)
	}
)


setMethod("plot", signature(x='RectangularHull', y='missing'), 
	function(x, ...) {
		sp::plot(x@polygons, ...)
	}
)


if (!isGeneric("rectHull")) {
	setGeneric("rectHull", function(p, ...)
		standardGeneric("rectHull"))
}	


setMethod('rectHull', signature(p='data.frame'), 
	function(p, n=1, dissolve=FALSE, crs=NULL, ...) {
		rh <- new('RectangularHull')
		rh@presence <- p
		rh@polygons <- .generateRectHulls(p, n, dissolve=dissolve)
		if (!is.null(crs)) {
			crs(rh@polygons) <- crs
		}
		return(rh)
	}
)


setMethod('rectHull', signature(p='matrix'), 
	function(p, ...) {
		rectHull(p=as.data.frame(p), ...)
	}
)

setMethod('rectHull', signature(p='SpatialPoints'), 
	function(p, ...) {
		pcrs <- list(...)$crs
		if (is.null(pcrs)) {
			pcrs <- crs(p)
		}
		rectHull(p=coordinates(p), crs=pcrs, ...)
	}
)


.generate_k_RectHulls <- function(xy, k, dissolve=FALSE) {
	if (k > (nrow(xy) / 2)) {
		stop('too many clusters (there should be at least two times as many points)')
	}
	cl <- kmeans(xy, k, 100)$cluster
	clusters <- unique(cl)
	subp <- list()
	for (i in clusters) {
		pts <- xy[cl==i, ]
		h <- .boundingRectangle(pts)
		subp <- c(subp, h)
	}
	aggregate(do.call(bind, subp), dissolve=dissolve)
}



.generateRectHulls <- function(xy, n=1, dissolve=FALSE) {
	xy <- unique(  stats::na.omit(xy[, 1:2]) )
    if (nrow(xy) < 2) { stop ('Insufficient number of points to make a Rectangular Hull; you need at least 2 unique points' ) }
    n <- pmax(1, round(n))
    n <- pmin(n, floor(nrow(xy) / 3))
    n <- unique(n)
	
	if (length(n) == 1) {
		if (n == 1) {
			r <- .boundingRectangle(xy)
		} else {
			r <- .generate_k_RectHulls(xy, n, dissolve=dissolve)
		}
	} else { # multiple number of clusters
		pols <- list()
		for (k in n) {
			pols <- c(pols, .generate_k_RectHulls(xy, k, dissolve=dissolve))
		}
		r <- do.call(bind, pols)
	}
	SpatialPolygonsDataFrame(r, data.frame(id=1:length(r)))
}


