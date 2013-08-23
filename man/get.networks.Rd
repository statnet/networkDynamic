%  File networkDynamic/man/get.slices.networkDynamic.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2013 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{get.networks}
\alias{get.networks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Obtain a list of collapsed networks sampled periodically from a networkDynamic object}
\description{
  Given a start time, end time, and increment (or vectors of onsets and termini) to express sampling intervals, return a list of collapsed networks from a networkDynamic object. 
}
\usage{
  get.networks(dnet, start = NULL, end = NULL, time.increment = NULL, onsets=NULL, termini=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dnet}{
A \code{networkDynamic} object with possible vertex, edge, network, and attribute spell information. 
}
\item{start}{ numeric value giving the start of the sampling interval}
\item{end}{ numeric value giving the end of the sampling interval}
\item{time.increment}{value for the offset (and duration) between sucessive samples. Will default to 1 if not otherwise specified}

\item{onsets}{ A numeric vector containing the onset times of the networks to be extracted.   This must be accompanied by \code{termini} of the same length.
}
\item{termini}{ A numeric vector containing the terminus times of the networks to be extracted.   This must be  accompanied by \code{onsets} of the same length.
}
  
\item{\dots}{ Additional arguments to \code{\link{network.collapse}} (such as \code{rule},\code{active.default},\code{retain.all.vertices}) }
}



\details{
The sampling ("slicing") intervals may be defined using either the \code{start}, \code{end}, and \code{time.increment} parameters, or by providing parallel vectors of \code{onsets} and \code{termini}. If values are not specefied but a \code{\link{net.obs.period}} attribute exists to describe the 'natural' sampling parameters, \code{start} and \code{end} will defult to the max an min of the observations element, with \code{time.increment} set to its corresponding value in the \code{net.obs.period}.
}
\value{
A \code{list} of static \code{network} objects corresponding to the specified sampling intervals of the \code{networkDynamic}
}

\author{
Kirk Li, Skye Bender-deMoll
}
\note{
See Note in \code{\link{network.collapse}}.
}


\seealso{
See Also as \code{\link{network.collapse}} for obtaining a slice of static network, \code{\link{network.extract}} for extracting sub-ranges of a networkDynamic, \code{\link{get.vertex.attribute.active}} for more on TEA attributes, and \code{\link{as.network.networkDynamic}} for stripping the the networkDynamic class from an object. 
}
\examples{
# create a networkDynamic with some basic activity and time extended attributes (TEA)
test <- network.initialize(5)
add.edges.active(test, tail=c(1,2,3), head=c(2,3,4),onset=0,terminus=1)
activate.edges(test,onset=3,terminus=5)
activate.edges(test,onset=-2,terminus=-1)
activate.edge.attribute(test,'weight',5,onset=3,terminus=4)
activate.edge.attribute(test,'weight',3,onset=4,terminus=5,e=1:2)

# obtain the list of network
list <- get.networks(test,start=0, end=5)

# aggregate over a longer time period with specified rule
list <- get.networks(test,start=0, end=6,time.increment=2,rule='latest')

# use 'at' style extraction of momentary slices via onsets & termini
list <- get.networks(test,onsets=0:5,termini=0:5)

# ensure that all networks returned will be the same size
list <- get.networks(test,onsets=0:5,termini=0:5,retain.all.vertices=TRUE)


# find out how many edges in each sampling point with apply
sapply(get.networks(test,start=0,end=5),network.edgecount)

# generate a list of matrices
lapply(get.networks(test,start=0,end=5),as.matrix)


}