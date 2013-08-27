\name{reconcile.vertex.activity}
\alias{reconcile.vertex.activity}
\alias{reconcile.edge.activity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Modify the activity spells of vertices to match incident edges or the other way around
}
\description{
When networkDynamic objects are created from real-world data it is often the case that activity information for vertices and edges may not come from the same source and may not match up exactly.  Vertices may be inactive when incident edges are active, etc.  The \code{reconcile.vertex.activity} function modifies the activity of a network's vertices acording to the \code{mode} specified.
}
\usage{
reconcile.vertex.activity(net, mode = "match.to.edges", edge.active.default = TRUE)

reconcile.edge.activity(net, mode="reduce.to.vertices", active.default=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{a \code{\link{networkDynamic}} object
}
  \item{mode}{ string indicating the method for reconciling
}
  \item{edge.active.default}{
boolean, should edges with no definied activity be considered active?}
  \item{active.default}{
boolean, should edges and nodes with no definied activity be considered active?

}
}
\details{

\itemize{
The \code{mode} argument takes the following values:
\item \code{match.to.edges} vertices will be modified so as to be only active when incident edges are active. Existing vertex spells are deleted.
\item \code{expand.to.edges} vertices activity will be expanded to include the activity periods of any incident edges (still permits isolated vertices). Existing vertex spells are preserved.
\item \code{encompass.edges} vertices activity will be modified so that it has a single spell beginning with the earliest incident edge activity, and encompasses the last edge activity. Existing vertex spells are deleted.

\item \code{match.to.vertices} edges will be modified so as to be active whenever all incident vertices are active. Existing
edge spells are deleted.
\item \code{reduce.to.vertices} edges will be modified so as to have no active spells when incident verticse are inactive. Existing edge spells are preserved.
}

}
\value{
  The input \code{networkDynamic} object is modified in place and returned invisibly.
}

\author{
skyebend, lxwang
}



\examples{
nd<-network.initialize(6)
add.edges.active(nd,tail=1:3,head=2:4,onset=1,terminus=3)
add.edges.active(nd,tail=4,head=1,onset=5,terminus=7)
add.edge(nd,tail=1,head=6)
# before
get.vertex.activity(nd,as.spellList=TRUE)
reconcile.vertex.activity(nd)
# after
get.vertex.activity(nd,as.spellList=TRUE)
}

