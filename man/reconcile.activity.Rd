\name{reconcile.vertex.activity}
\alias{reconcile.vertex.activity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Modify the activity spells of vertices to match incident edges or the other way around
}
\description{
When networkDynamic objects are created from real-world data it is often the case that activity information for vertices and edges may not come from the same source and may not match up exactly.  Vertices may be inactive when incident edges are active, etc.  The \code{reconcile.vertex.activity} function modifies the activity of a network's vertices acording to the \code{mode} specified.
}
\usage{
reconcile.vertex.activity(net, mode = "match.to.edges", edge.active.default = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{a \code{\link{networkDynamic}} object
}
  \item{mode}{ string indicating the method for reconciling
}
  \item{edge.active.default}{
boolean, should edges with no definied activity be considered active?
}
}
\details{

\itemize{
The \code{mode} argument takes the following values:
\item \code{match.to.edges} vertices will be modified so as to be \bold{only} active when incident vertices are active.

}

}
\value{
  The input \code{networkDynamic} object is modified in place and returned invisibly.
}

\author{
skyebend
}
\note{
not yet fully implemented, only one rule so far. Should have rule to expand (but not delete) activity to match edges
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

