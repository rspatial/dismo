# Author: Robert J. Hijmans
# Date : Febrary 2010
# Version 0.1
# Licence GPL v3


setClass('ConvexHull',
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


setMethod("polygons", "ConvexHull",
	function(obj) {
		obj@polygons
	}
)

setMethod("geometry", "ConvexHull",
	function(obj) {
		geometry(obj@polygons)
	}
)

setMethod("plot", signature(x='ConvexHull', y='missing'), 
	function(x, ...) {
		sp::plot(x@polygons, ...)
	}
)



if (!isGeneric("convHull")) {
	setGeneric("convHull", function(x, ...)
		standardGeneric("convHull"))
}	


setMethod('convHull', signature(x='matrix'), 
	function(x, n=1, crs=NA, ...) {
		ch <- new('ConvexHull')
		ch@presence <- data.frame(x)
		ch@polygons <- .generateConvexHulls(x, n, dissolve=FALSE)
		crs(ch@polygons) <- crs
		return(ch)
	}
)


setMethod('convHull', signature(x='data.frame'), 
	function(x, ...) {
		convHull(as.matrix(x), ...)
	}
)

setMethod('convHull', signature(x='SpatialPoints'), 
	function(x, ...) {
		convHull(coordinates(x), crs=x@proj4string, ...)
	}
)


.generate_k_ConvexHulls <- function(xy, k, dissolve=FALSE) {
	cl <- kmeans(xy, k, 100)$cluster
	clusters <- unique(cl)
	subp <- list()
	for (i in clusters) {
		pts <- xy[cl==i, ]
		h <- pts[chull(pts), ]
		h <- rbind(h, h[1,,drop=FALSE])
		r <- spPolygons(h)
		subp <- c(subp, r)
	}
	aggregate(do.call(bind, subp), dissolve=dissolve)
}



.generateConvexHulls <- function(xy, n=1, dissolve=FALSE) {
	xy <- unique(  stats::na.omit(xy[, 1:2]) )
    if (nrow(xy) < 3) { stop ('Insufficient number of points to make a Convex Hull; you need at least 3 unique points' ) }
    n <- pmax(1, round(n))
    n <- pmin(n, floor(nrow(xy) / 3))
    n <- unique(n)

	
	if (length(n) == 1) {
		if (n == 1) {
			h <- xy[chull(xy), ]
			h <- rbind(h, h[1,,drop=FALSE])
			r <- spPolygons(h)
		} else {
			r <- .generate_k_ConvexHulls(xy, n, dissolve=dissolve)
		}
	} else { # multiple number of clusters	
		pols <- list()
		for (k in n) {
			pols <- c(pols, .generate_k_ConvexHulls(xy, k, dissolve=dissolve))
		}
		r <- do.call(bind, pols)
	}
	SpatialPolygonsDataFrame(r, data.frame(id=1:length(r)))
}


