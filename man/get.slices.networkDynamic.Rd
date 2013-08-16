%  File networkDynamic/man/get.slices.networkDynamic.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2013 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{get.slices.networkDynamic}
\alias{get.slices.networkDynamic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Obtain the list of networks from networkDynamic}
\description{
  Given a specified time point (or interval), return a list of network extracted from networkDynamic object
}
\usage{
  get.slices.networkDynamic(dnet, onset=NULL, terminus=NULL, at=NULL, length=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dnet}{
A \code{networkDynamic} object with possible vertex, edge, network, and attribute spell information. 
}
\item{onset}{ optionally, the start of the specified interval.  This must be  accompanied by one of \code{terminus} or \code{length}.
}

  \item{terminus}{ optionally, the end of the specified interval.  This must be  accompanied by one of \code{onset} or \code{length}.
  }
  \item{length}{ optionally, the length of the specified interval. This must be  accompanied by one of \code{onset} or \code{terminus}.
  }
  \item{at}{ optionally, a single time point.}
\item{\dots}{ Additional arguments to \code{\link{network.collapse}} (such as \code{rule},\code{active.default},\code{retain.all.vertices}) }
}



\details{
If either (\code{onset=-Inf}) or (\code{terminus=Inf}), the function will stop with an error, since an infinite number of slices are not allowed.
}
\value{
A \code{list} object (consists of \code{network} object) corresponding to the specified time range of the \code{networkDynamic} argument. In addition, the summary of network statistics by time can be computed by calling \code{lapply} on the list.
For each value in the list, \code{NULL}: the edge is not activate at that slice; \code{NA}: the attribute is not activate at that slice, but the edge is activate; \code{Numerical Value}: both the edge and the attribute are activate at that slice.
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
list <- get.slices.networkDynamic(test,onset=0, terminus=5)

# obtain the list of network attribute values over time 
# (onset to terminus as specifed in get.slices.networkDynamic)
lapply(list,function(x)list.vertex.attributes(x))
lapply(list,function(x)list.edge.attributes(x))
lapply(list,function(x)get.edge.value(x,"weight"))

}