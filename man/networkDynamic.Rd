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
networkDynamic(base.net = NULL, edge.spells = NULL, vertex.spells = NULL, edge.toggles = NULL, vertex.toggles = NULL, start = NULL, end = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base.net}{
 A network (or network-coearceable) object which will be used to define network-level properties (directedness,etc) of the output network. When constructing from toggles, the edges in base.net give the initially active set of edges and verticies. that the activity states will be toggled from. 
}
  \item{edge.spells}{
Not currently implemented. A matrix or data.frame of spells specifying edge timing.
}
  \item{vertex.spells}{
Not currently implemented. A matrix or data.frame of spells specifying vertex timing.
}
  \item{edge.toggles}{
A matrix or data.frame of toggles giving a sequence of activation and deactivation times for toggles.  Columns are assumed to be toggle time, tail vertex id of the edge, and head vertex id of the edge.
}
  \item{vertex.toggles}{
Not currently implemented. A matrix or data.frame of toggles giving a sequence of activation and deactivation times for vertices. 
}
  \item{start}{
Optional argument to indicate the earliest time at which any changes in the network could have been observed or simulated; any spells before this time point are considered onset-censored.
}
  \item{end}{
Optional argument to indicate the latest time at which any changes in the network could have been observed or simulated; any spells after this time point are considered terminus-censored.
}
  \item{\dots}{
Additional arguments controling the creating of the network or processing of attached data objects.
}
}
\details{
This function provides ways to convert multiple forms of timing information for vertices and edges into network dynamic objects. 
}
\value{
A networkDynamic object with vertex and edge timing specified by the input data. 
}

\author{
statnet team
}
\note{
This function provides the features that were formerly called using as.networkDynamic.* syntax.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also as \code{\link{get.edge.activity}},\code{\link{get.vertex.activity}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (base.net = NULL, edge.spells = NULL, vertex.spells = NULL, 
    edge.toggles = NULL, vertex.toggles = NULL, start = NULL, 
    end = NULL, ...) 
{
    if (!is.null(vertex.spells)) {
        stop("Vertex spell conversion not yet implemented")
    }
    if (!is.null(edge.spells)) {
        stop("Edge spell conversion not yet implemented")
    }
    if (!is.null(vertex.toggles)) {
        stop("Vertex toggle conversion not yet implemented")
    }
    if (!is.null(edge.toggles) & !is.null(base.net)) {
        return(as.networkDynamic.network(base.net, toggles = edge.toggles))
    }
  }
}

