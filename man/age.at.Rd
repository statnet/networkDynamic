\name{age.at}
\alias{edges.age.at}
\alias{dyads.age.at}
\alias{vertices.age.at}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Age of active edges or vertices at a query time point 
}
\description{
Reports the age of edges or vertices at a specific time point. More precisely, the duration of time between the query time point and the onset of the activity spell active at that point. 
}
\usage{
edges.age.at(nD, at, e = seq_along(nD$mel), active.default = TRUE)
dyads.age.at(nD, at, tails, heads, active.default=TRUE,
                    format.out = c("vector", "edgelist", "matrix"))
vertices.age.at(nD, at, v=seq_len(network.size(nD)), active.default=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nD}{
 a \code{\link{networkDynamic}} object to be evaluated
}
  \item{at}{
numeric query time point which edge or vertex ages should be evaluated at
}
  \item{e}{
numeric vector of edges ids for which ages / durations should be reported
}

  \item{v}{
numeric vector of vertex ids for which ages / durations should be reported
}

  \item{tails}{
numeric vector of vertex ids incident on  the 'tail' of edges for which ages / durations should be reported 
}

  \item{heads}{
numeric vector of vertex ids incident on  the 'head' of edges for which ages / durations should be reported
}
   

  \item{active.default}{
logical, if \code{TRUE} edges or vertices with no activity specified will be considered always active (see \code{\link{is.active}})
}

  \item{format.out}{
   character value indicating out the output should be structured. \code{vector} returns output as a vector, \code{edgelist} returns a 3-column matrix in which the first columns are the tail and head vertex ids of the edge and the 3rd column is the age, \code{matrix} returns an adjacency matrix with the edge ages as values. 
  }
}
\details{
Edges or vertices that are not active at time \code{at} will return \code{NA}. For edges or vertices with multiple activity spells, this function \emph{does not} report the total duration of activity across all spells, only the duration from the start of the spell with which the \code{at} point intersects. 

\code{dyads.age.at} reports the age of edges corresponding to each dyad (tail,head).  It cannot be used with hypergraphic or multiplex networks because a pair of vertex ids may not uniquely correspond to an edge. If \code{tails} and \code{heads} are not specified, they will default to the tails and heads of all existing (but not necessarily active) edges in the network.  Ordering and index position should correspond to \code{\link[network]{valid.eids}}.
}
\value{
By default, a numeric vector indicating the age of the network element at the query time point, or NA if the element is not active or (in the case of edges) deleted. Elements of the vector return correspond to the values of \code{e} or \code{v} or \code{(tails,heads)} respectively. 
}

\author{
skyebend 
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{get.edge.activity}}, \code{\link{is.active}}
}
\examples{
# set up an example network for testing
test<-network.initialize(5)
add.edges(test,tail = 1:4,head=2:5 )
activate.edges(test,onset=0:2,terminus=c(2,3,3),e=1:3)
activate.vertices(test,v = 1:4,onset=-3:0,terminus=5:8)

# how old are each of the edges at time point 2.0 ?
edges.age.at(test,at=2.0)

# how old is vertex id 2 time point 3
vertices.age.at(test,at=3, v=2)

# how old are the edges [1,2] and [3,4] at time point 2
dyads.age.at(test,at=2,tails=c(1,3),heads=c(2,4))

dyads.age.at(test,at=2,format.out='matrix')

}