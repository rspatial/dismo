\name{gbm.plot}

\Rdversion{1.1}

\alias{gbm.plot}

\title{gbm plot}

\description{
Function to plot gbm response variables, with the option of adding a smooth representation of the response if requested additional options in this version allow for plotting on a common scale. Note hat fitted functions are centered by subtracting their mean.
}

\usage{
gbm.plot(gbm.object, variable.no=0, smooth=FALSE, rug=TRUE, n.plots=length(pred.names),
  common.scale=TRUE, write.title=TRUE, y.label="fitted function", x.label=NULL, 
  show.contrib=TRUE, plot.layout=c(3, 4), ...)
}

\arguments{
  \item{gbm.object}{a gbm object - could be one from gbm.step}
  \item{variable.no}{the var to plot - if zero then plots all}
  \item{smooth}{Logical. If \code{TRUE}, a smoothed version of the fitted function is added}
  \item{rug}{Logical. If \code{TRUE}, a rug of deciles is plotted}
  \item{n.plots}{plot the first n most important preds}
  \item{common.scale}{Logical. If \code{TRUE}, a common scale is used on the y axis}
  \item{write.title}{Logical. If \code{TRUE}, the plot gets a title}
  \item{y.label}{the default y-axis label}
  \item{x.label}{the default x-axis label}
  \item{show.contrib}{Logical. If \code{TRUE}, the contribution is shown on the x axis}
  \item{plot.layout}{define the default layout for graphs on the page}
  \item{\dots}{other arguments to pass to the plotting useful options include cex.axis, cex.lab, etc.}
}


\references{
Elith, J., J.R. Leathwick and T. Hastie, 2009. A working guide to boosted regression trees. Journal of Animal Ecology 77: 802-81
}

\author{
John R. Leathwick and Jane Elith
}

\keyword{ spatial }

