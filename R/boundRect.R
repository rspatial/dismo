
#based on code by whuber and Bangyou
#http://gis.stackexchange.com/questions/22895/how-to-find-the-minimum-area-rectangle-for-given-points/181883#181883

.boundingRectangle <- function(p) {
	if (inherits(p, 'Spatial')) {
		crs <- crs(p)
	} else {
		crs <- NA	
	}
	if (nrow(p) < 2) {
		stop("cannot make a rectangle from a single point")
	} else if (nrow(p) == 2) {
		xy <- .pointsToMatrix(p)
		r <- rbind(xy[1, ,drop=FALSE], cbind(xy[1,1], xy[2,2]), xy[2, ,drop=FALSE], cbind(xy[2,1], xy[1,2]))
		spPolygons(r, crs=crs)		
	} else {
		ch <- convHull(p)
		xy <- geom(polygons(ch))[, c('x', 'y')]
	}
	edges <- cbind(xy[-nrow(xy), ], xy[-1,])
	edgedir <- edges[, 1:2] - edges[,3:4]
    norms <- apply(edgedir, 1, function(x) sqrt(x %*% x)) # Edge lengths

    v <- diag(1/norms) %*% as.matrix(edgedir)      # Unit edge directions
    w <- cbind(-v[,2], v[,1])                      # Normal directions to the edges

    # Find the MBR
    x <- apply(xy %*% t(v), 2, range)              # Extremes along edges
    y <- apply(xy %*% t(w), 2, range)              # Extremes normal to edges
    areas <- (y[1,]-y[2,])*(x[1,]-x[2,])           # Areas
    k <- which.min(areas)                          # Index of the best edge (smallest area)

    # Form a rectangle from the extremes of the best edge
    r <- cbind(x[c(1,2,2,1,1),k], y[c(1,1,2,2,1),k]) %*% rbind(v[k,], w[k,])
	
	spPolygons(r, crs=crs)
}


