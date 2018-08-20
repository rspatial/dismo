\name{DistModel}

\Rdversion{1.1}

\docType{class}

\alias{DistModel-class}

\title{Class "DistModel"}
\description{
Parent class for a number of distribution models defined in the dismo package (those created by \code{\link{bioclim}, \link{domain}, \link{maxent} and \link{mahal}}). This is a virtual Class, no objects may be direclty created from it. }


\section{Slots}{
  \describe{
    \item{\code{presence}:}{presence data used}
    \item{\code{absence}:}{absence or background data used}
    \item{\code{hasabsence}:}{Logical indicating whether there is any absence data}
  }
}


\author{
Robert J. Hijmans
}

\keyword{classes}
