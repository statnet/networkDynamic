%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2013 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{networkDynamic}
\alias{networkDynamic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert various forms of network timing information into networkDynamic objects.
}
\description{
Converts various forms of temporal data (spell matrices, togles, lists of networks ) describing dynamics of vertices and edges into networkDynamic objects.
}
\usage{
networkDynamic(base.net = NULL, edge.toggles = NULL, vertex.toggles =NULL, 
                 edge.spells = NULL, vertex.spells = NULL,
                 edge.changes = NULL, vertex.changes = NULL,
                 network.list = NULL, onsets = NULL, termini = NULL,
                 vertex.pid = NULL, start = NULL, end = NULL, 
                 net.obs.period=NULL,verbose=TRUE,create.TEAs = FALSE,
                 edge.TEA.names=NULL,vertex.TEA.names=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base.net}{
 A network (or network-coearceable) object which will be used to define network-level properties (directedness,etc) of the output network. When constructing from toggles, the edges in base.net give the initially active set of edges and vertices that the activity states will be toggled from. Network and vertex attributes of \code{base.net} will be copied to output (unless they are overwritten by dynamic attributes with the same names) 
}
  \item{edge.spells}{
A matrix or data.frame of spells specifying edge timing. Assumed to be [onset,terminus,tail vertex.id, head vertex.id]. Any additional columns can loaded as dynamic attributes (see \code{edge.TEA.names})
}
  \item{vertex.spells}{
A matrix or data.frame of spells specifying vertex timing. Assumed to be [onset,terminus,vertex.id]
}
  \item{edge.toggles}{
A matrix or data.frame of toggles giving a sequence of activation and deactivation times for toggles.  Columns are assumed to be [toggle time, tail vertex id of the edge, head vertex id of the edge].
}
  \item{vertex.toggles}{
 A matrix or data.frame of toggles giving a sequence of activation and deactivation times for vertices. Column order assumed to be [time,vertex.id] 
}
 \item{edge.changes}{
   A matrix or data.frame of edge changes with at least 4 columns, assumed to be [time, tail, head, direction]
 }
 \item{vertex.changes}{
  A matrix or data.frame of vertex changes with at least 3 columns, assumed to be [time, vertex.id,direction]
 }
 \item{network.list}{
  a list of network objects assumed to describe sequential panels of network observations. Network sizes may vary if some vertices are only active in certain panels. See onsets, termini, vertex.pid. If base.net not specified, first element of list will be used as base.net. Network, vertex, and edge attributes will be converted to TEAs if \code{create.TEAs=TRUE}. 
 }
 \item{onsets}{
 an optional array of onset times to be assigned to the network panels of network.list. defaults to seq(from=0,length=length(network.list)-1) 
 }
 \item{termini}{
   an optional array of terminus times to be assigned to the network panels of network.list defaults to seq(from=1,length=length(network.list) 
 }
 \item{vertex.pid}{
 an optional name of a vertex attribute to be used as a unique vertex identifier when constructing from a network list with different sized networks.
 }
  \item{start}{
Optional argument to indicate the earliest time at which any changes in the network could have been observed or simulated; any spells before this time point are considered onset-censored.
}
  
  \item{end}{
Optional argument to indicate the latest time at which any changes in the network could have been observed or simulated; any spells after this time point are considered terminus-censored.
}
 \item{net.obs.period}{
  Optional argument. A structured list for providing additional information about when and how the network was observed. 
  }
 \item{verbose}{
 Logical, If TRUE (default), status message will be printed about the assumptions made in the conversion process. 
 }
 \item{create.TEAs}{If TRUE, Dynamic TEA attributes will be created corresponding to the static attributes appear on the network elements of \code{network.list}
 
 }
 \item{edge.TEA.names}{an optional vector of names for the dynamic (TEA) edge attributes to be imported from the extra columns of \code{edge.spells} (if \code{create.TEAs=TRUE})
 }
 \item{vertex.TEA.names}{an optional vector of names for the dynamic (TEA) vertex attributes to be imported from the extra columns of \code{vertex.spells} (if \code{create.TEAs=TRUE})
 }
  \item{\dots}{
Additional arguments controlling the creating of the network or processing of attached data objects.
}
}
\details{
This function provides ways to convert multiple forms of timing information for vertices and edges into network dynamic objects. 

If the timing information is provided in the form of a lists of networks (specified by the \code{network.list} argument ) the assumption is that each network is a discrete `panel' observation indicating the active set vertices and edges.  By default, each observation is assumed to span a unit interval, (so the 1st goes from 0 to 1, 2nd from 1-2, etc). However, the onset and termini of each observation panel can be explicitly specified via the \code{onsets} and \code{termini} arguments. If the networks in \code{network.list} vary in size, the name of a vertex attribute to be used as a persistent id  must be specified using the \code{vertex.pid} attribute so that the vertices in each network can be matched up correctly.  If \code{create.TEAs=TRUE}, dynamic attributes will be created for any (non-default) attributes appearing in the list of networks.  Otherwise, network and vertex attributes will be copied from the network specified via \code{base.net}.  If \code{base.net} is not specified, the first element of \code{network.list} will be used.  A \code{\link{net.obs.period}} will be constructed to describe the range of observations used to construct the network, or one can be specified directly using the \code{net.obs.period} argument. 

Alternatively, timing information for edges and vertices can be provided separately as matrices or data.frames in the forms of tables of spells, changes, or toggles.  For vertices, the arguments are \code{vertex.spells}, \code{vertex.changes} or \code{vertex.toggles}. The columns for each argument have a specific order and function, indicated in the argument definitions above. Edge arguments are named similarly \code{edge.spells}, \code{edge.changes} or \code{edge.toggles}. The vertex ids and `head' and `tail' ids must be integer index ids.   Network properties (number of vertices, bipartite, directedness, etc) can currently be specified by including a \code{base.net} argument to be used as a template, network and vertex attributes will be copied from \code{base.net} as well.  If only edge information is provided, the network size will be imputed from the set of edge endpoints.  In the \code{edge.toggles} case, the edges present in \code{base.net} provide the initial state to be toggled from. 

When constructing a network with separate sources of data for vertex and edge timing, edge and vertex activity will not be made consistent if input data is inconsistent -- edges may be active when their incident vertices are inactive, etc.  These situations can be detected using the \code{\link{network.dynamic.check}} function and possible resolved using one of the \code{\link{reconcile.activity}} functions.

Curently, dynamic attributes (TEAs, see \code{\link{
attribute.activity.functions}}) for edges or vertices(with onset and termination times matching those of the edge or vertex spell) can be loaded in by by setting \code{create.TEAs=TRUE} and including additional columns in the \code{edge.spells} or \code{vertex.spells} argument and specifying a vector of names for the attributes with \code{edge.TEA.names} or \code{vertex.TEA.names}. Note that when using a \code{\link{data.frame}} to pass in non-numeric attributes, the default behavior of converting strings to factors should be avoided. 


Specification is located at \url{https://statnet.csde.washington.edu/trac/wiki/NetworkDynamicConverterFunctions}


}
\value{
A networkDynamic object with vertex and edge timing specified by the input data. It will also print out information about the assumptions it makes while constructing the network.  
}

\author{
 Li Wang lxwang@uw.edu, skyebend, statnet team
}
\note{
This function provides the features that were formerly (in versions < 0.4) called using as.networkDynamic.* syntax. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also as \code{\link{get.edge.activity}},\code{\link{get.vertex.activity}}, \code{\link{network.dynamic.check}}
}
\examples{

# construct network out of a list of panels of varying sizes
# omiting missing day 25
data(windsurferPanels)
dynBeach<-networkDynamic(network.list=beach[-25], vertex.pid="vertex.names",
                        onsets=c(1:24,26:31),termini=c(2:25,27:32))

# read in tsv files for vertex and edge spells and 
# construct network of McFarland classroom interaction data
# see ?cls33_10_16_96 for more info about this data set

# read vertex data
rawVerts<-read.table(paste(path.package('networkDynamic'),
   "/extdata/cls33_10_16_96_vertices.tsv",sep=''),header=TRUE)

# peek at column headings to find ids and times
names(rawVerts)

# read in interation (edge) data
rawEdges<-read.table(paste(path.package('networkDynamic'),
  "/extdata/cls33_10_16_96_edges.tsv",sep=''),header=TRUE)

# peek at column headings to find ids and times
names(rawEdges)


# construct network using vertex and edge timing information
cls33 <-networkDynamic(vertex.spells=rawVerts[,c(3,4,1)],
                       edge.spells=rawEdges[,c(3,4,1,2)])

# add in the unchanging vertex attribute data
set.vertex.attribute(cls33,"sex",as.vector(rawVerts$sex))
set.vertex.attribute(cls33,"role",as.vector(rawVerts$role))

 \dontrun{  # takes 5 seconds, too slow for CRAN checks
# loop over edge data to add the dynamic attributes on the edge
for(r in 1:nrow(rawEdges)){
  # get the id of the edge from its tail and head
  eid <- get.edgeIDs(cls33,v=rawEdges$from_vertex_id[r],
                     alter=rawEdges$to_vertex_id[r])
  activate.edge.attribute(cls33,'interaction_type',rawEdges$interaction_type[r],
          onset=rawEdges$start_minute[r],terminus=rawEdges$end_minute[r],e=eid)
  activate.edge.attribute(cls33,'weight',rawEdges$weight[r],
          onset=rawEdges$start_minute[r],terminus=rawEdges$end_minute[r],e=eid)
}

# convert the set of newcomb panels with rank weights
data(newcomb)
newRankDyn <-networkDynamic(network.list=newcomb.rank)
get.network.attribute.active(newRankDyn,'title',at=3)
as.matrix(network.collapse(newRankDyn,at=5),attrname='rank')

}


}

