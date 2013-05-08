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
                 net.obs.period=NULL,verbose=TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base.net}{
 A network (or network-coearceable) object which will be used to define network-level properties (directedness,etc) of the output network. When constructing from toggles, the edges in base.net give the initially active set of edges and verticies. that the activity states will be toggled from. 
}
  \item{edge.spells}{
A matrix or data.frame of spells specifying edge timing. Assumed to be [onset,terminus,tail vertex.id, head vertex.id]
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
  a list of network objects assumed to describe sequential panels of network observations. Network sizes may vary if some vertices are only active in certain panels. See onsets, termini, vertex.pid. If base.net not specified, first element of list will be used as base.net. 
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
  Optional arugment. A structured list for providing additional information about when and how the network was observed. 
  }
 \item{verbose}{
 Logical, If TRUE (default), status message will be printed about the assumptions made in the conversion process. 
 }
  \item{\dots}{
Additional arguments controling the creating of the network or processing of attached data objects.
}
}
\details{
This function provides ways to convert multiple forms of timing information for vertices and edges into network dynamic objects. A magic fairy looks at the input data and constructs the appropriate network. Specification is located at \url{https://statnet.csde.washington.edu/trac/wiki/NetworkDynamicConverterFunctions}
}
\value{
A networkDynamic object with vertex and edge timing specified by the input data. 
}

\author{
 Li Wang lxwang@uw.edu, skyebend, statnet team
}
\note{
This function provides the features that were formerly called using as.networkDynamic.* syntax.  This version does not yet handle conversion of vertex, edge and network attributes of network.list items into TEA attributes, or importing TEA attributes using additional columns of input data.frames.  These functions are expected in the v0.5 release.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also as \code{\link{get.edge.activity}},\code{\link{get.vertex.activity}}
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
}


}

