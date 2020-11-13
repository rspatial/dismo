\name{gmap}

\alias{gmap}
\alias{Mercator}

\title{Get a Google map}

\description{
Retrieve a 'Google Map' that can be used as a background for plotting points and other spatial data.

The projection of the returned Raster object is "Mercator" (unless you use \code{lonlat=TRUE}), and other spatial data may need to be transformed before it can be plotted on top of the Google map. You can use the \code{Mercator} function to transform points from longitude/latitude to Mercator. For SpatialLines and SpatialPolygons objects, use \code{spTransform} in the \code{rgdal} package.

This function uses the Google static maps web-service, and is based on functions by Markus Loecher for the \code{RgoogleMaps} package. 
}

\usage{
gmap(x, exp=1, type='terrain', filename='', style=NULL, scale=1, zoom=NULL, 
      size=c(640, 640), rgb=FALSE, lonlat=FALSE, map_key, geocode_key, ...)

Mercator(p, inverse = FALSE)
}

\arguments{
  \item{x}{a textual locality description, or an Extent object (with longitude/latitude coordinates), or an object that can be coerced to one (such as a Raster* or Spatial* object), in any (known) coordinate system}
  \item{exp}{numeric. An expansion factor to enlarge (by multiplication) the extent specified by \code{x} }
  \item{type}{character. Choose from 'roadmap', 'satellite', 'hybrid', 'terrain' }
  \item{filename}{character. Filename (optional). You can open the resulting file in a GIS program }
  \item{style}{character. Additional style arguments. See \url{https://developers.google.com/maps/documentation/maps-static/overview?csw=1#StyledMapFeatures}.
  Note that certain style features do not work in combination with (the default) \code{type='terrain'} }
  \item{scale}{1 or 2. Using 2 doubles the number of pixels returned (and thus gives you better image quality if you need a large image)}
  \item{zoom}{integer between 0 (the whole world) to 21 (very small area), centered on the center of the extent}
  \item{size}{vector of two integers indicating the number of columns and rows that is requested (what is returned depends on other factors as well). Maximum values are \code{c(640, 640)}, so you can only select a smaller area than the default. Note that the number of pixels returned can be doubled by using \code{scale=2}}
  \item{rgb}{logical. If \code{TRUE}, a RasterBrick is returned with three layers (red, green, blue). This can be plotted with \code{\link{plotRGB}}}
  \item{lonlat}{logical. If \code{TRUE} the Raster object returned has a longitude/latitude CRS instead of Mercator}
  \item{map_key}{character. Your Google API key for mapping (and billing). See  https://developers.google.com/maps/documentation/javascript/get-api-key)}   
  \item{geocode_key}{character. Your Google API key for geocoding (and billing). Only relevant if \code{x} is a textual locality description. See  https://developers.google.com/maps/documentation/javascript/get-api-key)}   
  \item{...}{additional parameters }
  
  \item{p}{Points. A two-column matrix, or a SpatialPoints object}
  \item{inverse}{ Should the inverse projection be done (from Mercator to longitude/latitude?) }  
}

\value{
RasterLayer
}


\details{
If argument \code{x} is a textual locality description, the \code{\link[dismo]{geocode}} function is used to retrieve the extent that should be mapped.

Change the type to 'roadmap' if the map returned says "sorry we have no imagery here"; or use a larger extent.

The returned RasterLayer has a Mercator projection. To plot points (or lines or polygons) on top of it, these need to be transformed first.

A matrix of longitude/latitude data can be transformed with the Mercator function used in the example below. 'Spatial*' objects can be transformed with \code{spTransform}
p <- spTransform(x, "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
}

\author{Robert Hijmans and Sebastien Rochette, based on code by Markus Loecher, Sense Networks <markus at sensenetworks.com> in the RgoogleMaps package}

\examples{
\dontrun{
mymapkey = "pk-tHVbDiymfUL"
mygeokey = "Skxe99-adfKeax"

library(rgdal)

# from a maxtrix with lon/lat points
x <- runif(30)*10 + 40
y <- runif(30)*10 - 20
xy <- cbind(x, y)
g <- gmap(xy, type='hybrid', map_key=mymapkey)
plot(g, inter=TRUE)
points(Mercator(xy) , col='red', pch=20)

# or from an Extent object
e <- extent( -121.9531 , -120.3897 , 35.36 , 36.61956 )
# you can also get an Extent object by clicking on the map twice after using:
# drawExtent()
r <- gmap(e, map_key=mymapkey)
plot(r, interpolate=TRUE)

# transform points to Mercator for plotting on top of map:
pt <- matrix(c(-121, 36), ncol=2)
ptm <- Mercator(pt)
points(ptm, cex=3, pch=20, col='blue')
Mercator(ptm, inverse=TRUE)

# transform Spatial objects to Mercator for plotting on top of map
# here for points, but particularly relevant for lines and polygons
pt <- data.frame(pt)
coordinates(pt) <- ~X1 + X2
proj4string(pt) <-"+proj=longlat +datum=WGS84 +ellps=WGS84"
ptm2 <- spTransform(pt, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 
      +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
points(ptm, col='red', pch='x', cex=3)



# get a map using names
g = gmap('Australia', map_key=mymapkey, geocode_key=mygeokey)
plot(g, inter=TRUE)

gs = gmap('Sydney, New South Wales, Australia', type='satellite',
                map_key=mymapkey, geocode_key=mygeokey)
plot(gs, inter=TRUE)

gs = gmap('Sydney, Australia', type='satellite', exp=3, 
				map_key=mymapkey, geocode_key=mygeokey)
plot(gs, inter=TRUE)

gs = gmap('Sydney, Australia', type='hybrid', zoom=10, scale=2,
				map_key=mymapkey, geocode_key=mygeokey)
plot(gs, inter=TRUE)

# styles:
g <- gmap("Brooklyn", style="feature:road.local|element:geometry|hue:0x00ff00|saturation:100
  &style=feature:landscape|element:geometry|lightness:-100", type='roadmap', 
  map_key=mymapkey, geocode_key=mygeokey)
plot(g)
}
}

\keyword{ spatial }

