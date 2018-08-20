\name{ecolim}

\alias{ecolim}
\alias{ecolim,matrix,matrix-method}
\alias{EcoLim-class}
\alias{predict,EcoLim-method}


\title{Ecolim model}

\description{
Simple generic limiting factor based model, in the tradition of the PLANTGRO model (Hackett, 1991)
}

\usage{
\S4method{ecolim}{matrix,matrix}(x, y, extrapolate=TRUE, ...)
}
 
 

\arguments{
  \item{x}{numeric matrix with driver variables (each column has values for the variables). Values have to be in ascending order}
  \item{y}{numeric matrix with responses (between 0 and 1), one column for each column in \code{x}}
  \item{extrapolate}{logical. Should the model extrapolate beyond the extremes of x? If \code{TRUE} the value of \code{y} at the closest data extreme in \code{x} is used, else \code{NA} is returned for such records} 
  \item{...}{Additional arguments. None implemented}
}


\author{
Robert J. Hijmans
}


\references{
Hackett, C., 1991. PLANTGRO, a software package for coarse prediction of plant growth. CSIRO, Melbourne, Australia
}

\examples{
# get predictor variables
fnames <- list.files(path=paste(system.file(package="dismo"), '/ex', sep=''), 
              pattern='grd', full.names=TRUE ) 
env <- stack(fnames)

bio1 <- c(200,250,400,450)
bio12 <- c(0,1000, 3000, 4000)
r1 <- c(0, 1, 1, 0)
r2 <- c(0, 0, 1, 1)
x <- cbind(bio1, bio12)
y <- cbind(r1, r2)

e <- ecolim(x, y) 
plot(e, lwd=2, col='red')
p <- predict(e, env)
plot(p)

# no extrapolation:
ef <- ecolim(x, y, extrapolate=FALSE) 
pf <- predict(ef, env)
plot(pf)


occurence <- paste(system.file(package="dismo"), '/ex/bradypus.csv', sep='')
occ <- read.table(occurence, header=TRUE, sep=',')[,-1]
fold <- kfold(occ, k=5)
occtest <- occ[fold == 1, ]
occtrain <- occ[fold != 1, ]
bg <- randomPoints(env, 1000)


\dontrun{
# An approach to optimize the values based on
# some known presences and (here random) absences
# for the same species as in the maxent example

# intial parameters
v <- c(200, 250, 400, 450, 0, 1000, 3000, 4000)

# function to be minimized
f <- function(p) {
	x[] <- p
	# numbers must go up
	if ( any(x[-1,] < x[-nrow(x), ]) ) return(Inf)
	e <- ecolim(x, y) 
	# we are minimizing, hence 1-AUC
	1-evaluate(e, p=occtrain, a=bg, x=env)@auc
}

# patience...
set.seed(0)
z <- optim(v, f)

x[] <- z$par
eco <- ecolim(x, y) 
evaluate(eco, p=occtest, a=bg, x=env)

set.seed(0)
pwd <- pwdSample(occtest,bg,occtrain)
ptest <- occtest[!is.na(pwd),]
atest <- bg[na.omit(pwd),]
evaluate(eco, p=ptest, a=atest, x=env)

p2 <- predict(eco, env)
plot(p2)
}
}

\keyword{spatial}


