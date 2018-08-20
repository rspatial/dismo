# Author: Robert J. Hijmans
# Date : July 2016
# Version 0.1
# Licence GPL v3


.generateCircleHull <- function(xy, lonlat, ...) {

	crs  <- crs(xy)
	
	if (missing(lonlat)) {
		if (is.na(crs)) {
			lonlat <-  couldBeLonLat(xy)
			if (lonlat) {
				warning('crs unknown, assuming lonlat')
			}
		} else {
			lonlat <- isLonLat(crs)
		}
	}
	
	xy <- na.omit(unique(.pointsToMatrix(xy, checkLonLat=lonlat)))
	stopifnot(nrow(xy) > 1)

	# first getting the points on the covex hull
	xy <- xy[chull(xy),]

	f <- function(p) { max(pointDistance(rbind(p), xy, lonlat=lonlat)) }
	p <- stats::optim(colMeans(xy), f)
	if (is.na(crs)) crs <- CRS(as.character(NA))
	b <- buffer(SpatialPoints(rbind(p$par), proj4string=crs), width=p$value, quadsegs=45)
	SpatialPolygonsDataFrame(b, data.frame(x=p$par[1], y=p$par[2], r=p$value), match.ID = FALSE)
}





setClass('CircleHull',
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


setMethod("polygons", "CircleHull",
	function(obj) {
		obj@polygons
	}
)

setMethod("geometry", "CircleHull",
	function(obj) {
		geometry(obj@polygons)
	}
)

setMethod("plot", signature(x='CircleHull', y='missing'), 
	function(x, ...) {
		sp::plot(x@polygons, ...)
	}
)



if (!isGeneric("circleHull")) {
	setGeneric("circleHull", function(p, ...)
		standardGeneric("circleHull"))
}	


setMethod('circleHull', signature(p='matrix'), 
	function(p, crs=NA, ...) {
		ch <- new('CircleHull')
		ch@presence <- data.frame(p)
		lonlat <- isLonLat(crs)
		if (is.na(lonlat)) {
			ch@polygons <- .generateCircleHull(p)
		} else {
			ch@polygons <- .generateCircleHull(p, lonlat=lonlat)		
		}
		crs(ch@polygons) <- crs
		return(ch)
	}
)


setMethod('circleHull', signature(p='data.frame'), 
	function(p, ...) {
		circleHull(as.matrix(p), ...)
	}
)

setMethod('circleHull', signature(p='SpatialPoints'), 
	function(p, ...) {
		circleHull(coordinates(p), crs=p@proj4string, ...)
	}
)


