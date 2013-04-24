%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2013 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{network.collapse}
\alias{network.collapse}
\alias{\%k\%}
\title{
 Convert a time range of a networkDynamic object into a static network object.
}
\description{
First performs an extraction of the passed networkDynamic object with the specified time range. Computes some crude summary attributes for edge and vertex spells (\code{activity.count}, \code{activity.duration}), extracts appropriate values from any dynamic attributes and stores them as static attributes. Then removes all temporal information and returns a plain network object.
}
\usage{
network.collapse(dnet, onset = NULL, terminus = NULL, at = NULL, length = NULL, 
                 rule = c("any", "all"), active.default = TRUE, 
                 retain.all.vertices=FALSE, ...)
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
  \item{rule}{ a text string for defining \dQuote{active} for this call:  \code{any} if elements active at any time during the interval are to be used, or \code{all} if elements must be active over the entire interval. }
  \item{active.default}{ logical; should elements without an activity attribute be regarded as active by default?}
  \item{retain.all.vertices}{ logical; should the extracted network retain all vertices, ignoring the vertex activity spells of x in order to ensure that the network returned has the same size as dnet?}
  \item{\dots}{ Possible additional arguments (not yet invented) to handle aggregation of attributes. }
}

\details{
If no temporal arguments are specified, defaults to collapsing entire time range of the input network (\code{onset=-Inf,terminus=Inf}). Original network is not modified. 
}
\value{
A new 'static' \code{network} object corresponding to the specified time range of the \code{networkDynamic} argument.  If the original network contained dynamic TEA attributes (i.e. 'weight.active'), the appropriate (if possible) value is queried and stored as a non-TEA attribute ('weight').
}

\author{
Skye Bender-deMoll
}
\note{
Inappropriate attribute values may be returned when query spells with any duration are used for continuous time networks (i.e. anything other than 'at') or query spells that don't line up with the discrete units for discrete time networks.  If a query spell intersects with multiple attribute values, a warning will be produced and only the earliest value will be used (see \code{\link{get.vertex.attribute.active}}). Also, the duration values returned for edges do not account for any potential `censoring' of observations in the original network. This function may be quite expensive if the network contains lots of attributes. For many tasks it is possible to avoid collapsing the network by using the various activate and deactivate methods for vertices, edges and attributes. 
}


\seealso{
See Also as \code{\link{network.extract}} for extracting sub-ranges of a networkDynamic, \code{\link{get.vertex.attribute.active}} for more on TEA attributes, and \code{\link{as.network.networkDynamic}} for stripping the the networkDynamic class from an object. 
}
\examples{
# create a network with some basic activity
test<-network.initialize(5)
add.edges.active(test, tail=c(1,2,3), head=c(2,3,4),onset=0,terminus=1)
activate.edges(test,onset=3,terminus=5)
activate.edges(test,onset=-2,terminus=-1)

# collapse the whole thing
net <-network.collapse(test)
is.networkDynamic(net)
get.vertex.attribute(net,'activity.duration')
get.edge.value(net,'activity.count')
get.edge.value(net,'activity.duration')

# add a dynamic edge attribute
activate.edge.attribute(test,'weight',5,onset=3,terminus=4)
activate.edge.attribute(test,'weight',3,onset=4,terminus=5)

# collapse with an interval query
net3<-network.collapse(test,onset=3,terminus=4)
get.edge.value(net3,'weight')

# note that if we use a query that intersects mutiple 
# attribute values it will generate a warning. 
# try commented line below:

# net3<-network.collapse(test,onset=3,terminus=5)

# but should be safe from attribute issues when
# collapsing with a point query
net3<-network.collapse(test,at=3)
get.edge.value(net3,'weight')

# can use operator version for point query instead
net3<-test\%k\%4.5
get.edge.value(net3,'weight')



}
