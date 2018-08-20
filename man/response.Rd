\name{response}

\alias{response}
\alias{response,DistModel-method}
\alias{response,MaxEntReplicates-method}
\alias{response,ANY-method}


\title{response plots}


\description{
Generate 'response plots', i.e. single variable response curves for a model
}


\usage{
response(x, ...) 
}


\arguments{
  \item{x}{ Model object that inherits from 'DistModel', e.g. 'MaxEnt'. Also works for some other models (e.g. GLM) }
  \item{...}{ Additional arguments. See Details }  
}


\details{  
\tabular{rll}{  
\tab \code{var} \tab Variable to be plotted (if NULL, all variables will be plotted) \cr
\tab \code{at}  \tab Function to indicate at what level the other variables should be. E.g. median (the default), mean, min, max. Note that currently the function fails when 'mean' is used and one of the variables is a factor. Instead of a function you can also supply a single-row matrix with values for each of the variables. If NULL, partial response plots as are computed (this can take a lot of time). Partial response plots show, for each variable, the average prediction for all data points that were used to fit the model (or other data, if supplied), for 10 regularly spaced values of the variable. \cr
\tab \code{range} \tab 'pa' (default) or 'p'. Show responses for the range of the presence data (p) or presence and absence (background) data (pa) for models that use absence data.\cr
\tab \code{expand} \tab  percentage to expand the range of values with. Default is 10 \cr
\tab \code{rug} \tab Logical. If \code{TRUE} (the default) a 'rug' of deciles is plotted on the horizontal axes) \cr
\tab \code{data} \tab data.frame with data to use, normally this is the data used to fit the model and does not needs to be supplied as they are available from the model object\cr
\tab \code{fun} \tab  predict function. The default is "predict"; but you may change this to e.g., function(x, y, ...) predict(x, y, type='response', ...).\cr
\tab \code{...}  \tab Additional graphical parameters  \cr
}
}


\seealso{
\code{\link[dismo]{density}, \link[dismo]{plot}, \link[dismo]{pairs}}
}


\value{
Used for the side-effect of a plot
}

\author{
Robert J. Hijmans
}

\keyword{spatial}
