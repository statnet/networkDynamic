\name{add.methods}
\alias{add.edges.networkDynamic}
\alias{add.vertices.networkDynamic}
\alias{add.edge.networkDynamic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PID-aware versions of functions for adding edges and vertices to networkDynamic objects
}
\description{
The methods (\code{add.vertices.networkDynamic}, \code{add.edge.networkDynamic}, \code{add.edges.networkDynamic}) override their network-package counterparts in order to permit assigning \link{persistent.ids} to newly-added elements if the pid is defined.  They can be defined by the user with the \code{vertex.pids} argument, or, if not specified, a unique random id will be generated. (Note that any new values added by default to a \code{vertex.names} pid will not be numeric.)
}
\usage{
\method{add.vertices}{networkDynamic}(x, nv, vattr = NULL, last.mode = TRUE, 
    vertex.pid = NULL, ...)

\method{add.edges}{networkDynamic}(x, tail, head, names.eval = NULL, vals.eval = NULL, 
    edge.pid = NULL, ...)
    
\method{add.edge}{networkDynamic}(x, tail, head, names.eval = NULL, vals.eval = NULL, 
    edge.check = FALSE, edge.pid = NULL, ...)    
    
}

%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{x}{an object of class \code{network} or \code{networkDynamic} (see \code{\link[network:add.vertices]{add.vertices}} in network)}
  \item{nv}{number of vertices to be added. (see \code{\link[network:add.vertices]{add.vertices}} in network)}
  \item{vattr}{optionally, a list of attributes with one entry per new vertex. (see \code{\link[network:add.vertices]{add.vertices}} in network), not interpreted as a dynamic attribute}
  \item{last.mode}{logical; should the new vertices be added to the last (rather than the first) mode of a bipartite network? (see \code{\link[network:add.vertices]{add.vertices}} in network)}
  \item{vertex.pid}{a vector of vertex.pids to be assigned to the newly-added vertices}
  \item{edge.pid}{a vector of edge.pids to be assigned to the newly-added edges}
  \item{names.eval}{see \code{\link[network:add.edges]{add.edges}} in network), not interpreted as a dynamic attribute}
  \item{vals.eval}{see \code{\link[network:add.edges]{add.edges}} in network), not interpreted as a dynamic attribute}
  \item{edge.check}{see \code{\link[network:add.edges]{add.edges}} in network)}
  \item{head}{see \code{\link[network:add.edges]{add.edges}} in network)}
  \item{tail}{see \code{\link[network:add.edges]{add.edges}} in network)}
  \item{...}{possible additional arguments}
}
\details{
The networkDynamic versions of these methods call the network versions internally. See \link{persistent.ids} for additional details about the PID mechanism. 
}
\value{
\itemize{
  \item{\code{add.verticies} (invisibly) returns a reference to the network with the new vertices added. Pids will be added/created if a \code{vertex.pid} attribute is set.}
  \item{\code{add.edge} and \code{add.edges} (invisibly) return a reference to the network with the new edges added. Pids will be added/created if a \code{vertex.pid} attribute is set.}
}
}
\references{
A specification document: \url{https://statnet.csde.washington.edu/trac/wiki/PersistentIdProposal}
}
\author{
lxwang, skyebend, the statnet team
}
\note{
  Adding edges via the extraction/replacement operators \code{[,]} bypasses the pid code and will break the edge pids defined for the network. Similarly, \code{\link{add.vertices.active}} and \code{\link{add.edges.active}} do not yet support including pids. 
}


\seealso{
See also \code{\link{persistent.ids}}, \code{\link[network:add.vertices]{add.vertices}}, \code{\link[network:add.edge]{add.edge}}, \code{\link[network:add.edges]{add.edges}} in network.
}
\examples{
# add vertices while using vertex.names as pids
nd <- as.networkDynamic(network.initialize(3))
set.network.attribute(nd,"vertex.pid","vertex.names")
add.vertices(nd,nv=2,vertex.pid=c(4,5)) # specify pids for two new vertices
network.vertex.names(nd) # peek at pids

# add vertices and edges w/ auto-generated pids
nd <- as.networkDynamic(network.initialize(3))
initialize.pids(nd)
get.vertex.attribute(nd,'vertex.pid') # peek at pids
add.vertices(nd,nv=2) # silently generate pids for vertices
get.vertex.attribute(nd,'vertex.pid') # peek at pids
add.edges(nd,1,2) # silently generate a pid for the edge
get.edge.attribute(nd,'edge.pid')

}

