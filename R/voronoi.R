# Author: Robert J. Hijmans
# Date : Febrary 2010
# Version 1.0
# Licence GPL v3

# adapted from code by Carson Farmer
# http://www.carsonfarmer.com/?p=455



if (!isGeneric("voronoi")) {setGeneric("voronoi", function(x, ...) standardGeneric("voronoi"))
}	


setMethod('voronoi', signature(x='ANY'), 
function(x, ext=NULL, eps=1e-09, ...){

	if (!requireNamespace('deldir')) { stop('first install the deldir package') }

	dat <- NULL
	sp <- FALSE
	if (inherits(x, 'Spatial')) {
		if (.hasSlot(x, 'data')) {
			dat <- slot(x, 'data')
		}
		prj <- proj4string(x)
		sp <- TRUE
		xy <- coordinates(x)
		dups <- duplicated(xy)
		if (any(dups)) {
			xy <- xy[!dups, ,drop=FALSE]
			dat <- dat[!dups, ,drop=FALSE]
		}
	} else {
		xy <- stats::na.omit(x[, 1:2])
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
		polys <- SpatialPolygons(polys, proj4string=sp::CRS(prj))
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
)

