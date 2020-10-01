# Author: Robert J. Hijmans
# Date : Febrary 2010
# Version 1.0
# Licence GPL v3

# adapted from code by Carson Farmer
# http://www.carsonfarmer.com/?p=455

voronoi <- function(xy, ext=NULL, eps=1e-09, ...){

	if (!requireNamespace('deldir')) { stop('first install the deldir package') }

	dat <- NULL
	sp <- FALSE
	if (inherits(xy, 'Spatial')) {
		if (.hasSlot(xy, 'data')) {
			dat <- slot(xy, 'data')
		}
		prj <- proj4string(xy)
		sp <- TRUE
		xy <- coordinates(xy)
		dups <- duplicated(xy)
		if (any(dups)) {
			xy <- xy[!dups, ,drop=FALSE]
			dat <- dat[!dups, ,drop=FALSE]
		}
	} else {
		xy <- stats::na.omit(xy[, 1:2])
		xy <- unique(xy)
	}
	
	
	if (!is.null(ext)) {
		ext <- as.vector(extent(ext))
	}
	
	z <- deldir::deldir(xy[,1], xy[,2], rw=ext, eps=eps, suppressMsge=TRUE)
	index <- z$ind.orig
	w <- deldir::tile.list(z)
	polys <- vector(mode='list', length=length(w))
	for (i in seq(along=polys)) {
		pc <- cbind(w[[i]]$x, w[[i]]$y)
		pc <- rbind(pc, pc[1,])
		polys[[i]] <- Polygons(list(Polygon(pc)), as.character(index[i]))
	}
	if (sp) {
		polys <- SpatialPolygons(polys, proj4string=CRS(prj))
	} else {
		polys <- SpatialPolygons(polys)
	}

	if (is.null(dat)) {
		dat <- data.frame(id=index)
	} else {
		dat <- dat[index, ,drop=FALSE]
	}
	rownames(dat) <- row.names(polys)
	polys <- SpatialPolygonsDataFrame(polys, data=dat)
	return(polys)
}

