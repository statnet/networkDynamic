%  File networkDynamic/man/network.extract.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{network.extract}
\alias{network.extract}
\alias{\%t\%}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Temporal Extracts/Cross-sections of Dynamically Extended Network Objects }
\description{
  Given a specified time point (or interval), return a reduced network
  at the given point or over the given interval.
}
\usage{
network.extract(x, onset = NULL, terminus = NULL, length = NULL, at = NULL,
    rule = c("any", "all"), active.default = TRUE, retain.all.vertices = FALSE,
    trim.spells=FALSE)

x \%t\% at
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{\link[network]{network}}. }
  \item{onset}{ optionally, the start of the specified interval.  This must be  accompanied by one of \code{terminus} or \code{length}.}
  \item{terminus}{ optionally, the end of the specified interval.  This must be  accompanied by one of \code{onset} or \code{length}.}
  \item{length}{ optionally, the length of the specified interval. This must be  accompanied by one of \code{onset} or \code{terminus}.}
  \item{at}{ optionally, a single time point.}
  \item{rule}{ a text string for defining \dQuote{active} for this call:  \code{any} if elements active at any time during the interval are to be used, or \code{all} if elements must be active over the entire interval. }
  \item{active.default}{ logical; should elements without an activity attribute be regarded as active by default? }
  \item{retain.all.vertices}{ logical; should the extracted network retain all vertices, ignoring the vertex activity spells of x in order to ensure that the network returned has the same size as x?}
  \item{trim.spells}{ logical; should the spells of vertices, edges and their attributes in the extracted network be trimmed to match the query range? (Sensible thing to do, but could be expensive)}
}
\details{
  For the purposes of extraction, edges are only considered active over some interval if: the edge itself is active over this time period, and both of the incident vertices are also active over the given time period.
  
  When \code{retain.all.vertices=FALSE} (the default), the function has the following behavior:
 \itemize{
	 \item If \code{at} is used to specify the spell of interest, the returned network consists of all edges and vertices
  active at that time point.  
	\item If an interval is supplied to specify the spell (via \code{onset} and one of \code{terminus} or \code{length}), edges and vertices active
  over the specified interval are used.   If \code{rule="any"}, then edges and vertices active at any time during the interval are returned; otherwise, only those active during the entire period are returned. 
 }
Vertices in the extracted network will have the same order as the original network, with inactive vertices removed. 
 
When \code{retain.all.vertices=TRUE}
\itemize{
   \item{All vertices are preserved in the output net to preserved network size and ids.}
   \item{if \code{trim.spells=TRUE}, 'retained' vertices will be marked as inactive (they will have 'null' spell (Inf,Inf)) }
   \item{Edges that are active (by the specified \code{rule}) during the specified spell are included in the returned network (same as above.)}
   \item{Edges with one or more inactive vertices still will be removed (even if the vertex is being 'retained' to preserve network size)}
   
  }
  \code{\%t\%} (the temporal cross-section operator) is  a
  simplified (and less flexible) wrapper for \code{network.extract} that
  returns the network of active vertices and edges at a given time point.
}
\value{
  A \code{networkDynamic} object containing the active edges and vertices for the specified spell, when \code{retain.all.vertices=FALSE}, otherwise the network object containing all vertices, and only those edges
  active for the specified spell.
}

\note{
 Note that only active vertices are included by default (\code{retain.all.vertices=FALSE}).  As a result, the size of
  the extracted network may be smaller than the original. Vertex and edge ids will be translated, but may not correspond to their original values.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Carter T. Butts \email{buttsc@uci.edu}}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{is.active}}, \code{\link{activity.attribute}}, \code{\link{network.extensions}}, and \code{\link[network:get.inducedSubgraph]{get.inducedSubgraph}} for a related non-temporal version}
\examples{

triangle <- network.initialize(3)    # create a toy network

# add edges with activity 
add.edges.active(triangle,onset=0,terminus=10,tail=1,head=2)               # add an edge between vertices 1 and 2
add.edges.active(triangle,onset=0,length=4,tail=2,head=3)               # add a more edges
add.edges.active(triangle,at=4.5,tail=3,head=1)

# specify some vertex activity
activate.vertices(triangle,onset=0,terminus=10)
deactivate.vertices(triangle,onset=1,terminus=2,v=3)

degree<-function(x){as.vector(rowSums(as.matrix(x)) 
  + colSums(as.matrix(x)))} # handmade degree function

degree(triangle) # degree of each vertex, ignoring time

degree(network.extract(triangle,at=0))
degree(network.extract(triangle,at=1)) # just look at t=1
degree(network.extract(triangle,at=2))
degree(network.extract(triangle,at=5))

# watch out for empty networks! they are just an empty list
t10 <- network.extract(triangle,at=10)
t10

# notice difference between 'at' syntax and 'onset,terminus'
# when network is not in discrete time
degree(network.extract(triangle,at=4))
degree(network.extract(triangle,onset=4,terminus=5))

# the \%t\% (time) operator is like an alias for the 'at' extraction syntax
degree(triangle\%t\%4)

par(mfrow=c(2,2))
#show multiple plots
plot(triangle,main='ignoring dynamics',displaylabels=TRUE)
plot(network.extract(triangle,onset=1,terminus=2),main='at time 1',displaylabels=TRUE)
plot(network.extract(triangle,onset=2,terminus=3),main='at time 2',displaylabels=TRUE)
plot(network.extract(triangle,onset=5,terminus=6),main='at time 5',displaylabels=TRUE)

}

