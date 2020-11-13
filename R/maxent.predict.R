# Author: Robert J. Hijmans
# Date: December 2009
# Version 0.1
# Licence GPL v3


if (!isGeneric("predict")) {
	setGeneric("predict", function(object, ...)
		standardGeneric("predict"))
}	


setMethod('predict', signature(object='MaxEntReplicates'), 
	function(object, x, ext=NULL, filename='', args="", ...) {
		MEversion <- .getMeVersion()

		n <- length(object@models)
		if (filename != '') {
			filename <- trim(filename)
			fxt <- extension(filename)
			extension(filename) <- ''
			fname <- paste(filename, '_', 1:n, fxt, sep='')
		} else {
			fname <- rep('', n)
		}
		lst <- list()
		for (i in 1:n) {
			lst[[i]] <- predict(object@models[[i]], x, ext=ext, filename=fname[i], args=args, ...)
		}
		return(stack(lst))
	}
)



.predictSpatRaster <- function(object, x, ext=NULL, args="", filename='', ...) {

	args <- c(args, "")
	lambdas <- paste(object@lambdas, collapse='\n')
	variables <- colnames(object@presence)
		
	mxe <- rJava::.jnew("mebridge") 		
	args <- c("-z", args)
	tst <- rJava::.jcall(mxe, "S", "testPredictArgs", lambdas, args) 
	if (!is.null(tst)) {
		stop("args not understood:\n", tst)
	}
	filename <- trimws(filename)
		
		
	if (! all(colnames(object@presence)  %in%  names(x) )) {
		stop('missing layers (or wrong names)')
	}

	if (!is.null(ext)) {
		x <- terra::crop(x, ext)
	}
	out <- terra::rast(x, nlyr=1)
	names(out)  <- "maxent"
	ncols <- terra::ncol(out)
	if (!terra::readStart(x)) { stop(x@ptr$messages$getError()) }
	on.exit(terra::readStop(x))
			
	overwrite <- list(...)$overwrite 
	if (is.null(overwrite)) overwrite <- FALSE
	wopt <- list(...)$wopt 
	if (is.null(wopt)) wopt <- list()
			
	b <- terra::writeStart(out, filename, overwrite, wopt)
	for (i in 1:b$n) {
		rowvals <- terra::readValues(x, b$row[i], b$nrows[i], 1, ncol(x), TRUE, FALSE)
		rowvals <- rowvals[,variables,drop=FALSE]
		res <- rep(NA, times=nrow(rowvals))
		rowvals <- stats::na.omit(rowvals)
		if (length(rowvals) > 0) {
			rowvals[] <- as.numeric(rowvals)
			p <- rJava::.jcall(mxe, "[D", "predict", lambdas, rJava::.jarray(colnames(rowvals)), rJava::.jarray(rowvals, dispatch=TRUE), args) 

			naind <- as.vector(attr(rowvals, "na.action"))
			if (!is.null(naind)) {
				res[-naind] <- p
			} else {
				res <- p
			}
			res[res == -9999] <- NA
			terra::writeValues(out, res, b$row[i], b$nrows[i])
		}
		terra::writeStop(out)
	}
	return(out)
}






setMethod('predict', signature(object='MaxEnt'), 
	function(object, x, ext=NULL, args="", filename='', ...) {

		MEversion <- .getMeVersion() 

		if (inherits(x, "SpatRaster")) {
			return(.predictSpatRaster(object, x, ext, args, filename, ...))
		}
		
		args <- c(args, "")
		lambdas <- paste(object@lambdas, collapse='\n')
		variables <- colnames(object@presence)
		
		mxe <- rJava::.jnew("mebridge") 		
		args <- c("-z", args)
		tst <- rJava::.jcall(mxe, "S", "testPredictArgs", lambdas, args) 
		if (!is.null(tst)) {
			stop("args not understood:\n", tst)
		}

		filename <- trim(filename)
		
		if (inherits(x, "Raster")) {
			
			if (! all(colnames(object@presence)  %in%  names(x) )) {
				stop('missing layers (or wrong names)')
			}
			
			out <- raster(x)
			if (!is.null(ext)) {
				out <- crop(out, ext)
				firstrow <- rowFromY(x, yFromRow(out, 1))
				firstcol <- colFromX(x, xFromCol(out, 1))
			} else {
				firstrow <- 1
				firstcol <- 1
			}
			ncols <- ncol(out)
		
			
			if (!canProcessInMemory(out, 3) & filename == '') {
				filename <- rasterTmpFile()
			}
			
			if (filename == '') {
				v <- matrix(ncol=nrow(out), nrow=ncol(out))
				inMemory <- TRUE
			} else {
				out <- writeStart(out, filename=filename, ... )
				inMemory <- FALSE
			}


				tr <- blockSize(out, n=nlayers(x)+2)
				pb <- pbCreate(tr$n, ...)	
			
				for (i in 1:tr$n) {
					rr <- firstrow + tr$row[i] - 1
					rowvals <- getValuesBlock(x, row=rr, nrows=tr$nrows[i], firstcol, ncols)
					rowvals <- rowvals[,variables,drop=FALSE]
					res <- rep(NA, times=nrow(rowvals))
					rowvals <- stats::na.omit(rowvals)
					if (length(rowvals) > 0) {
						rowvals[] <- as.numeric(rowvals)
						p <- rJava::.jcall(mxe, "[D", "predict", lambdas, rJava::.jarray(colnames(rowvals)), rJava::.jarray(rowvals, dispatch=TRUE), args) 

						naind <- as.vector(attr(rowvals, "na.action"))
						if (!is.null(naind)) {
							res[-naind] <- p
						} else {
							res <- p
						}
						res[res == -9999] <- NA
					}	
				
					if (inMemory) {
						res = matrix(res, nrow=ncol(out))		
						cols = tr$row[i]:(tr$row[i]+dim(res)[2]-1)
						v[, cols] <- res
					} else {
						out <- writeValues(out, res, tr$row[i])
					}
					pbStep(pb, i) 
				} 
			
			pbClose(pb)
			if (inMemory) {
				out <- setValues(out, as.vector(v))
			} else {
				out <- writeStop(out)
			}

		} else { 
		
		
			if (inherits(x, "Spatial")) {
				x <- as.data.frame(x)
			}
			
			if (! all(colnames(object@presence) %in% colnames(x))) {
				stop('missing layers (or wrong names)')
			}
			
			
			x <- x[,variables,drop=FALSE]
			if (class(x) == 'data.frame') {
				for (i in 1:ncol(x)) {
					if (class(x[,i]) == 'factor') {
						x[,i] <- as.numeric(as.character(x[,i]))
					} else if (class(x[,i]) == 'character') {
						x[,i] <- as.numeric(x[,i])
					}
				}
			}
			
			out <- rep(NA, times=nrow(x))
			
			x <- stats::na.omit(x)
			if (nrow(x) > 0) {
				x <- as.matrix(x)
				x[] <- as.numeric(x)
				p <- rJava::.jcall(mxe, "[D", "predict", lambdas, rJava::.jarray(colnames(x)), rJava::.jarray(x, dispatch=TRUE), args) 
				p[p == -9999] <- NA
				naind <- as.vector(attr(x, "na.action"))
				if (!is.null(naind)) {
					out[-naind] <- p
				} else {
					out <- p
				}
			} 
		}
		#try( file.remove(lambdas), silent=TRUE )
		out
	}
)

