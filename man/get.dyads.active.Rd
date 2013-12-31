\name{get.dyads.active}
\alias{get.dyads.active}
\title{
Return the set of vertex ids of edges active at a given time
}
\description{
Constructs a matrix, essentially an edgelist, of the pairs of vertices connected by edges active within the query spell.
}
\usage{
get.dyads.active(nD, onset = NULL, terminus = NULL, length = NULL, at = NULL, 
                rule = c("any", "all"), active.default = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nD}{
networkDynamic object to be queried
}
  \item{onset}{
  optional numeric value giving onset time of query spell
}
  \item{terminus}{
  optional numeric value giving terminus time of query spell
}
  \item{length}{
  optional numeric value giving length (duration) of query spell
}
  \item{at}{
  optional numeric value giving single time point to query
}
  \item{rule}{
a character string indicating the definition of "active" for this query: \code{any} (default) to define active as any active spell during the interval, or \code{all} to define active as being active over the entire interval.
}
  \item{active.default}{
  logical; should elements without an activity attribute be regarded as active by default? Default to \code{TRUE}
}
}
\details{
Uses \code{\link{is.active}} internally.  The ordering of matrix rows is arbitrary.  The ordering of vertices in non-directed networks corresponds to how they were set in the data structure (i.e. does not enforce i > j) If no edges are active or exist, a matrix with zero rows will be returned. 
}
\value{
Returns a two-column numeric matrix in which the first column gives a tail vertex.id and the second column gives the head vertex.id and each row corresponds to the existence of an active relationship between the vertices for the given query spell. 
}

\author{
skyebend 
}
\note{
This function does not support hypergraphic networks. 
}


\seealso{
See also \code{\link{is.active}}
}
\examples{
 data(windsurfers)
 get.dyads.active(windsurfers,onset=2,terminus=3)
}
