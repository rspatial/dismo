# Author: Robert J. Hijmans
# License GPL3
# Version 1.0
# October 2010

geocode <- function(x, oneRecord=FALSE, extent=NULL, progress='', geocode_key,...) {

	if (missing(geocode_key)) {
		stop("you need to supply a Google API key")
	}

	ntry <- list(...)$ntry
	if (is.null(ntry)) ntry <- 10
	reps <- min(ntry, 10)
	x <- as.character(x)
	xx <- unique(x)
	xx <- data.frame(ID=1:length(xx), place=xx)
	resall <- .geocode(xx$place, oneRecord=oneRecord, extent=extent, progress=progress, api_key=geocode_key)

	k <- which(is.na(resall[,3]))
	if (length(k) > 0 & reps > 1) {
		n <- 0
		for (i in 2:reps) {
			print(paste('try', i, '...'))
			flush.console()
			j <- which(is.na(resall[,2]))
			if (length(j) == 0) break
			ids <- unique(resall[j,1])
			res <- .geocode(xx[ids, 'place'], oneRecord=oneRecord, extent=extent, progress=progress, api_key=geocode_key)
			k <- which(!is.na(res[,2]))
			if (length(k) == 0) {
				if (n == 2) break
				n <- n + 1
			} else {
				n <- 0
			}
			if (oneRecord) {
				res$ID <- j[match(res$ID, 1:length(j))]
				resall[j,] <- res
			} else {
				resall <- resall[-j, ,drop=FALSE]
				res$ID <- j[match(res$ID, 1:length(j))]
				resall <- rbind(resall, res)
			}
		}
	}
	x <- data.frame(ID=1:length(x), originalPlace=x)
	resall$ID <- NULL
	res <- merge(x, resall, by='originalPlace', all.x=TRUE)
	rownames(res) <- NULL
	res <- res[order(res$ID), , drop=FALSE]
	res$ID <- NULL
	res
}


.geocode <- function(x, oneRecord=FALSE, extent=NULL, progress='', api_key, ...) {
	
	tmpfile <- paste0(tempfile(), '.json')
	burl <- "https://maps.google.com/maps/api/geocode/json?address="
	res1 <- data.frame(matrix(NA, ncol=8, nrow=1))
	colnames(res1) <- c('ID', 'interpretedPlace', 'longitude', 'latitude', 'xmin', 'xmax', 'ymin', 'ymax')
	res <- res1[-1, ,drop=FALSE]
	pb <- pbCreate(length(x), progress)
	for (i in 1:length(x)) {
		r <- x[i]
		r <- gsub(', ', ',', r)
		r <- gsub(' ,', ',', r)
		r <- trim(r)
		if (length(r) > 0 & !is.na(r)) {
			r <- gsub(' ', '+', r)
			if (is.null(extent)) {
				gurl <- paste(burl, r, "&sensor=false", sep="")
			} else {
				e <- extent(extent)
				extent <- paste(e@ymin,',',e@xmin,'|',e@ymax,',',e@xmax,sep='')
				gurl <- paste(burl, r, "&bounds=", extent, "&sensor=false", sep="")	
				gurl <- iconv(gurl, to='UTF-8')
			}
			gurl <- paste0(gurl, "&key=", api_key)
			test <- try (download.file(gurl, tmpfile, quiet=TRUE))
			json <- scan(tmpfile, what='character', quiet=TRUE, sep='\n',  encoding = "UTF-8")
			js <- jsonlite::fromJSON(json)
			if (js$status != "OK") {
				w <- res1
				w[1] <- i				
			} else {
				p <- js$results
				n <- nrow(p)
				place <- rep(NA, n)
				location <- matrix(ncol=2, nrow=n)
				viewport <- matrix(ncol=4, nrow=n)
				bounds <- matrix(ncol=4, nrow=n)
				for (j in 1:n) {
					plc <-  paste0(p[j,1][[1]][,1], collapse=", ")
					place[j] <- ifelse(is.null(plc), "", plc)
					geometry = p[j,3]
					location[j,] <- as.numeric(c(geometry$location$lng, geometry$location$lat))
					viewport[j,] <- as.numeric(c(geometry$viewport$southwest$lng, geometry$viewport$northeast$lng, geometry$viewport$southwest$lat, geometry$viewport$northeast$lat) )
					bnds <- as.numeric(c(geometry$bounds$southwest$lng, geometry$bounds$northeast$lng, geometry$bounds$southwest$lat, geometry$bounds$northeast$lat) )
					if (length(bnds)==4) bounds[j,] <- bnds
				}

				w <- cbind(viewport, bounds)
				w[,c(1,3)] <- pmin(w[,c(1,3)], w[,c(1,3)+4], na.rm=TRUE)
				w[,c(2,4)] <- pmax(w[,c(2,4)], w[,c(2,4)+4], na.rm=TRUE)
				w <- w[,1:4,drop=FALSE]
				if (oneRecord & nrow(w) > 1) {
					f <- apply(w, 2, range)
					g <- apply(location, 2, mean)
					w <- data.frame(i, NA, g[1], g[2], f[1,1], f[2,2], f[1,3], f[2,4])
				} else {
					w <- data.frame(i, place, location, w)
				}
				colnames(w) <- colnames(res1)
			}
		} else {
			w <- res1
			w[1] <- i
		}
		res <- rbind(res, w)
		pbStep(pb, z) 
	} 
	pbClose(pb)

	da <- pointDistance(res[,3:4], res[,c(5,7)], longlat=T)
	db <- pointDistance(res[,3:4], res[,c(6,8)], longlat=T)
	res$uncertainty <- round(pmin(da, db))
	xx <- data.frame(ID=1:length(x), originalPlace=x)
	
	merge(res, xx, by='ID')
}


#a = .geocode('San Jose, Mexico', oneRecord=F, api_key=placekey)

 
 