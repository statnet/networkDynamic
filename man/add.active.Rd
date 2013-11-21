%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2013 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{add.vertices.active}
\alias{add.vertices.active}
\alias{add.edges.active}
\title{
add.verticies.active, add.edges.active
}
\description{
Convenience functions for adding a set of verticies (or edges) and setting them to be active in a single call
}

\usage{
\method{add.vertices}{active}(x, nv, vattr = NULL, last.mode = TRUE,
            onset = NULL, terminus = NULL, length = NULL, at = NULL, ...)

\method{add.edges}{active}(x, tail, head, names.eval = NULL, vals.eval = NULL, 
         onset = NULL, terminus = NULL, length = NULL, at = NULL, ...)
}
\arguments{
  \item{x}{ an object of class \code{\link{network}} or \code{\link{networkDynamic}}. }
  \item{nv}{the number of vertices to add}
  \item{tail}{a vector of vertex IDs corresponding to the tail (source, ego) of each edge to be added}
  \item{head}{a vector of vertex IDs corresponding to the head (target, alter) of each edge to be added}
  
  \item{onset}{ an optional vector of time points that specifies the starts of the interval(s). This must be accompanied by one of \code{terminus} or \code{length}. }
  \item{terminus}{ an optional vector of time points that specifies the ends of the interval(s). This must be accompanied by one of \code{onset} or \code{length}. }
  \item{length}{ an optional vector of interval lengths for the interval(s). This must be accompanied by one of \code{onset} or \code{terminus}. }
  \item{at}{ optional, one or more time points to be activated. }
  \item{names.eval}{optional list of length equal to the number of edges, with each element containing a list of names for the attributes of the corresponding edge. not currently interpreted in a dynamic context, but passed directly to \code{\link{add.edges}}}
  \item{vals.eval}{an optional list of lists of edge attribute values (matching \code{names.eval}). Not currently interpreted in a dynamic context, but passed directly to \code{\link{add.edges}}}
  \item{vattr}{optionally, a list of attributes with one entry per new vertex. not currently interpreted in a dynamic context, but passed directly to \code{\link{add.vertices}}}
  \item{last.mode}{logical; should the new vertices be added to the last (rather than the first) mode of a bipartite network?}
  \item{...}{possible future additional arguments}
}
\details{
Essentially a wrapper for a call to \code{\link{add.vertices}} and \code{\link{activate.vertices}}  or \code{\link{add.edges}} and \code{\link{activate.edges}} when setting up a network object. These are not the S3 methods that their name appears to imply, since there is no "active" class.  See \code{\link{add.edges.networkDynamic}}, etc.
}
\value{
The passed in network object with class set to \code{\link{networkDynamic}} and the specified number of new vertices or edges added and activated
}

\author{
Ayn Leslie-Cook \email{aynlc3@uw.edu}}

\note{Order of arguments was changed in version 1.9 for S3 method consistency. Does not currently support the multiple-vertex head- and tail-sets of \code{add.edges} or \code{add.edge}.}


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

