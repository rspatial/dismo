\name{gbm.perspec}

\Rdversion{1.1}

\alias{gbm.perspec}

\title{
gbm perspective plot
}

\description{
Takes a gbm boosted regression tree object produced by gbm.step and
plots a perspective plot showing predicted values for two predictors as specified by number using x and y.
Values for all other variables are set at their mean by default but values can be specified by giving a list consisting of the variable name
and its desired value, e.g., c(name1 = 12.2, name2 = 57.6)
}

\usage{
gbm.perspec(gbm.object, x = 1, y = 2, pred.means = NULL, x.label = NULL, x.range = NULL,
 y.label = NULL, z.label = "fitted value", y.range = NULL, z.range = NULL, 
 leg.coords = NULL, ticktype = "detailed", theta = 55, phi = 40, smooth = "none",
 mask = FALSE, perspective = TRUE, ...)
}

\arguments{
  \item{gbm.object}{object of class gbm}
  \item{x}{the first variable to be plotted}
  \item{y}{the second variable to be plotted}
  \item{pred.means}{allows specification of values for other variables}
  \item{x.label}{allows manual specification of the x label}
  \item{x.range}{manual range specification for the x variable}
  \item{y.label}{and y label}
  \item{z.label}{default z label}
  \item{y.range}{and the y}
  \item{z.range}{allows control of the vertical axis}
  \item{leg.coords}{can specify coords (x, y) for legend}
  \item{ticktype}{specifiy detailed types - otherwise "simple"}
  \item{theta}{rotation }
  \item{phi}{and elevation }
  \item{smooth}{controls smoothing of the predicted surface}
  \item{mask}{controls masking using a sample intensity model}
  \item{perspective}{controls whether a contour or perspective plot is drawn}
  \item{\dots}{allows the passing of additional arguments to plotting routine useful options include shade, ltheta, lphi for controlling illumination and cex for controlling text size - cex.axis and cex.lab have no effect}
}


\references{
Elith, J., J.R. Leathwick and T. Hastie, 2009. A working guide to boosted regression trees. Journal of Animal Ecology 77: 802-81
}

\author{
John R. Leathwick and Jane Elith
}

\keyword{ spatial }

