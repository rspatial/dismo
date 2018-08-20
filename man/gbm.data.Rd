\name{Anguilla data}

\alias{Anguilla_train}
\alias{Anguilla_test}
\alias{Anguilla_grids}

\docType{data}

\title{Anguilla australis distribution data}

\description{
A number of sites with presence or absence of the short-finned eel (Anguilla australis) in New Zealand, and environmental data at these sites; and gridded data of the environmental variables for the study area. 

\tabular{llll}{  
type  \tab variable name \tab values \tab mean and range \cr
Reach \tab LocSed \tab weighted average of proportional cover of bed sediment \tab 1=mud, 2=sand, 3=fine gravel; 4=coarse gravel; 5=cobble; 
6=boulder; 7=bedrock; 3.77, 1 to 7 \cr
Segment \tab SegSumT \tab Summer air temperature (degrees C) \tab 16.3, 8.9 to 19.8 \cr
 \tab SegTSeas \tab  Winter air temperature (degrees C), normalised with respect to SegJanT \tab  0.36, -4.2 to 4.1 \cr
 \tab SegLowFlow \tab segment low flow (m3/sec), fourth root transformed \tab 1.092, 1.0 to 4.09 \cr
Downstream \tab DSDist \tab distance to coast (km) \tab  74, 0.03 to 433.4 \cr
 \tab DSDam \tab presence of known downstream obstructions, mostly dams \tab 0.18, 0 or 1 \cr
 \tab DSMaxSlope \tab  maximum downstream slope (degrees) \tab  3.1, 0 to 29.7 \cr
Upstream 
/ catchment \tab USAvgT \tab  average temperature in catchment (deg C) compared to segment, normalised with respect to SegJanT \tab  -0.38, -7.7 to 2.2 \cr
 \tab USRainDays \tab days/month with rain greater than 25 mm \tab 1.22, 0.21 to 3.30 \cr
 \tab USSlope \tab average slope in the upstream catchment (degrees) \tab 14.3, 0 to 41.0 \cr
 \tab USNative \tab  area with indigenous forest (proportion) \tab  0.57, 0 to 1 \cr
 \tab Fishing method \tab  fishing method in five classes: electric, net, spot, trap & mixture \tab NA \cr
}
}

\usage{
data(Anguilla_train)
data(Anguilla_test)
data(Anguilla_grids)
}

\references{
Elith, J., J.R. Leathwick and T. Hastie, 2009. A working guide to boosted regression trees. Journal of Animal Ecology 77: 802-81
}

\author{
John R. Leathwick and Jane Elith
}

\keyword{datasets}

