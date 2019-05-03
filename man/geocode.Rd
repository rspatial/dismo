\name{geocode}

\alias{geocode}

\title{Georeferencing with Google}

\description{
A wrapper around the Google geocoding web-service. It returns 0 to n matches. It is important to be as precise as possible, e.g. always include the country in the locality description. 

}

\usage{geocode(x, oneRecord=FALSE, extent=NULL, progress='', geocode_key, ...)}

\arguments{
  \item{x}{A vector of locality descriptions}
  \item{oneRecord}{Logical. If \code{TRUE} a single record for each item in x is returned. If the API returned multiple records, the values of this record are computed by averaging the coordinates and taking the union of all bounding boxes }
  \item{extent}{An Extent object, or an object that can be coerced to one, to bias the search towards that region}
  \item{progress}{Character. Valid values are "" (no progress indicator), "text" or "window"}
  \item{geocode_key}{character. Your Google API key for geocoding (and billing). See  https://developers.google.com/maps/documentation/geocoding/get-api-key)}
  \item{...}{additional arguments (currently none implemeted)}
}

\value{
\code{data.frame} with the following fields:

\item{originalPlace}{the locality description as provided (in argument \code{x})}
\item{interpretedPlace}{the locality as interpreted by the Google API}
\item{lon}{longitude}
\item{lat}{latitude}
\item{lonmin}{minimum longitude of the bounding box}
\item{lonmax}{maximum longitude of the bounding box}
\item{latmin}{minimum latitude of the bounding box}
\item{latmax}{maximum latitude of the bounding box}
\item{uncertainty}{distance from \code{c(lon, lat)} to the farthest corner of the bounding box}
}


\note{It is important to compare fields \code{originalPlace} and \code{interpretedPlace} as the Google interpretation of a (perhaps vague) locality description can be very speculative}

\author{Robert J. Hijmans}

\examples{
\dontrun{
geocode(c('1600 Pennsylvania Ave NW, Washington DC', 'Luca, Italy', 'Kampala'))
geocode(c('San Jose', 'San Jose, Mexico'))
geocode(c('San Jose', 'San Jose, Mexico'), oneRecord=TRUE)
}
}

\keyword{ spatial }

