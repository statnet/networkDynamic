\name{add.vertices.active}
\alias{add.vertices.active}
\alias{add.edges.active}
\title{
add.verticies.active, add.edges.active
}
\description{
Convienience functions for adding a set of verticies (or edges) and setting them to be active in a single call
}

\usage{
add.vertices.active(x, nv, onset = NULL, terminus = NULL, length = NULL, at = NULL)

add.edges.active(x, onset = NULL, terminus = NULL, length = NULL, at = NULL, tail, head)
}
\arguments{
  \item{x}{ an object of class \code{\link{network}} or \code{\link{networkDynamic}}. }
  \item{nv}{the number of vertices to add}
  \item{tail}{a vector of vertex IDs corresponding to the tail (source, ego) of each edge to be added}
  \item{head}{a vector of vertex IDs corresponding to the head (target, alter) of each edge to be added}
  
  \item{onset}{ an optional vector of timepoints that specifies the starts of the interval(s). This must be accompanied by one of \code{terminus} or \code{length}. }
  \item{terminus}{ an optional vector of timepoints that specifies the ends of the interval(s). This must be accompanied by one of \code{onset} or \code{length}. }
  \item{length}{ an optional vector of interval lengths for the interval(s). This must be accompanied by one of \code{onset} or \code{terminus}. }
  \item{at}{ optional, one or more time points to be activated. }
}
\details{
Essentially a wrapper for a call to \code{\link{add.vertices}} and \code{\link{activate.vertices}}  or \code{\link{add.edges}} and \code{\link{activate.edges}} when setting up a network object. 
}
\value{
The passed in network object with class set to \code{\link{networkDynamic}} and the specified number of new vertices or edges added and activated
}

\author{
Ayn Leslie-Cook \email{aynlc3@uw.edu}}

\note{Does not currently support the multiple-vertex head- and tail-sets of \code{add.edges} or \code{add.edge}}


\seealso{
 See Also as \code{\link{activate.vertices}}, \code{\link{activate.edges}},\code{\link{add.vertices}},\code{\link{add.edges}}
}
\examples{
nw <- network.initialize(5)
activate.vertices(nw,onset=0,terminus=10)
network.size(nw)  # just 5 nodes
# add some new nodes with times
add.vertices.active(nw,2,onset=10,terminus=12)
network.size(nw)  # now we have 7 nodes

# add 2 edges edges, and activate them
add.edges(nw, tail=c(1,2),head=c(2,3))
activate.edges(nw,onset=0,terminus=10,e=1:2)

# instead add and activate at the same time
add.edges.active(nw, tail=c(3,4),head=c(4,5),onset=10,terminus=12)
}

