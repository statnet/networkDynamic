%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2013 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{persistent.ids}
\alias{persistent.ids}
\alias{vertex.pid.check}
\alias{edge.pid.check}
\alias{get.vertex.id}
\alias{get.vertex.pid}
\alias{get.edge.id}
\alias{get.edge.pid}
\alias{initialize.pids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Use and manipulate persistent ids of network elements
}
\description{
Persistent ids (pids) are unique values attached to vertices or edges which remain unchanged throughout network manipulation and extraction processes. The ``vertex.pid'' of a vertex is an overall data-set id, unlike the ``vertex.id'' which is an index dependent on network size.  These functions provide ways to look up vertex.pids from vertex.ids (and the other way around) and also provide necessary modifications to some standard network functions to safely work with pids. 
}
\usage{
get.vertex.id(nd, pid)

get.vertex.pid(nd, id)

get.edge.id(nd, pid)

get.edge.pid(nd, id)

initialize.pids(nd)

vertex.pid.check(nd)

edge.pid.check(nd)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
   a \code{network} or \code{networkDynamic} object to be modified 
  }
  \item{pid}{persistent id(s) of the network element(s) for which the network-specific id(s) should be returned}
  \item{id}{network-specific (index) id(s)  of the network element for which the persistent id(s) should be returned}
}
\details{
The persistent id functionality is an optional feature of \code{networkDynamic} objects. If a network-level attribute named \code{vertex.pid} exists, its value is required to be the name of a vertex attribute that can safely be used as a vertex.pid. If it is set to \code{NULL}, pids will be ignored.   A vertex.pid must have a unique value for all vertices. Persistent ids for edges function in the same way, except the attribute is named \code{edge.pid}.

Some existing network code uses the \code{vertex.names} attributes of networks as a persistent id without checking that it is unique. It is possible to indicate that \code{vertex.names} can safely be used as a vertex.pid by setting \code{vertex.pid} to \code{'vertex.names'}

The function \code{initialize.pids} can be used to create a set of pids on all existing vertices (named \code{vertex.pid} and edges named \code{edge.pid}). The pids are currently initialized with pseudo-random hex strings using the \code{\link{tempfile}} function (something like \code{'4ad912252bc2'}). It is especially useful if called after a network object as been constructed and before any extractions are performed. 

The element addition functions (\code{add.vertices}) override their network-package counterparts in order to permit assigning pids to newly-added elements if the pid is defined.  They can be defined by the user with the \code{vertex.pids} argument, or, if not specified, a unique random id will be generated. (Note that any new values added by default to a \code{vertex.names} pid will not be numeric.)

Some of the import/conversion methods may set pids. See \code{\link{network}}.

User-specified pids are checked for uniqueness. The the current auto-generated pid implementation produces ids that are unique within the current network.  They are also \emph{almost} certain to be unique within an R session (so that vertices will have a unique id if added and removed) and quite likely across sessions, but we need more details on the \code{tempfile}'s implementation.

With the exception of the "check" utilities, all of these functions modify their network argument in place. 
}
\value{
Get methods:
\itemize{
  \item{\code{get.vertex.id} returns the vertex.id(s) of vertices corresponding to the vertex.pid(s) provided.}
  \item{\code{get.vertex.pid} returns the vertex.pid(s) of vertices corresponding to the vertex.id(s) provided.}
  \item{\code{get.edge.id} returns the edge.id(s) of edge corresponding to the edge.pid(s) provided.}
  \item{\code{get.edge.pid} returns the edge.pid(s) of edges corresponding to the edge.id(s) provided.}
}
Each of the above return \code{NA} values where no match is found.

\itemize{
  \item{\code{vertex.pid.check} throws an error if the \code{vertex.pid} is found to not meet specifications. Otherwise returns \code{TRUE} or \code{FALSE} and gives a warning if vertex.pid does not exist} 
  \item{\code{edge.pid.check} throws an error if the \code{edge.pid} is found to not meet specifications. Otherwise returns \code{TRUE} or \code{FALSE} and gives a warning if edge.pid does not exist}
}

}
%\references{
%A specification document: \url{https://statnet.csde.washington.edu/trac/wiki/PersistentIdProposal}
%}
\author{
lxwang, skyebend, the statnet team
}
\note{
  Adding edges via the extraction/replacement operators \code{[,]} bypasses the pid code and will break the edge pids defined for the network. Similarly, \code{\link{add.vertices.active}} and \code{\link{add.edges.active}} do not yet support including pids. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link[network:add.vertices]{add.vertices}} in network.
}

\examples{

# use vertex.names as a persistent id
net<-network.initialize(5)
set.network.attribute(net, 'vertex.pid','vertex.names')

# find original vertex corresponding to vertex in smaller extracted net
haystack<-network.initialize(30)
activate.vertices(haystack,v=10:20)

# hide a needle somewhere in the haystack
set.vertex.attribute(haystack,'needle',TRUE,v=sample(1:30,1))

# set up the persistand ids with defaults
initialize.pids(haystack)

# some hay is removed over time ...
newstack<-network.extract(haystack,at=100,active.default=FALSE)
network.size(newstack)
# we find the needle!
needleId <-which(get.vertex.attribute(newstack,'needle'))
needleId

# which vertex is the corresponding one in original stack?
oldId<-get.vertex.id(haystack,get.vertex.pid(newstack,needleId))
oldId

# check if we got it right..
get.vertex.attribute(haystack,'needle')[oldId]


# one reason you wouldn't want to use ordinary vertex.names
net<-network.initialize(3)
add.vertices(net,3)
network.vertex.names(net)

# but if you make it a persistant id, new names will be created
net<-network.initialize(3)
set.network.attribute(net,'vertex.pid','vertex.names')
add.vertices(net,3)
network.vertex.names(net)

# try with edges and add/remove vertices
net <-network.initialize(10)
add.edges(net,1:9,2:10)
set.edge.attribute(net,'test',"you found me!",e=7)
initialize.pids(net)
changed<-net
add.vertices(changed,5)
delete.vertices(changed,c(1,3,5,15))
delete.edges(changed,eid=1:3)

# which edge in changed corresponds to edge 7 in net?
network.edgecount(changed)
get.edge.id(changed,get.edge.pid(net,7))
# actually, they are the same because of NULL edges in edgelist
get.edge.attribute(changed,'test',unlist=FALSE)[[7]]
# however, the ids of the vertices have changed
changed$mel[[7]]$inl
net$mel[[7]]$inl

# do they still match up?
get.vertex.pid(changed,changed$mel[[7]]$inl)==get.vertex.pid(net,net$mel[[7]]$inl)


}
