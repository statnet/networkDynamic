%  File networkDynamic/man/delete.activity.matrices.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{get.timing.info}
\alias{get.change.times}
\alias{get.edge.activity}
\alias{get.vertex.activity}
\title{ Retrieve the timing information present in a networkDynamic. }
\description{
  These methods return either the activity spells of each vertex or edge
  or the unique time points at which edges and vertices become active or
  inactive. 

  Finds all of the unique time points at which the edges and/or vertices
 become active or inactive.
}
\usage{
get.change.times(x, vertex.activity=TRUE, edge.activity=TRUE, ignore.inf=TRUE)
get.vertex.activity(x, v=1:network.size(x))
get.edge.activity(x, e=1:length(x$mel))
}
\arguments{
  \item{x}{ an object, of class \code{network} or \code{networkDynamic}. }
  \item{vertex.activity}{ logical, whether the times at which vertices become
    active or inactive is to be returned. }
  \item{edge.activity}{ logical, whether the times at which edges become
    active or inactive is to be returned. }
  \item{ignore.inf}{logical, whether Inf value will be ignored or not. }
  \item{e}{ IDs for the edges whose activity spells are wanted. }
  \item{v}{ IDs for the vertices whose activity spells are wanted. }
}
\value{
  For the 'get.change.times' function, the default settings return a
  vector of all of the finite time points at which any edge or any
  vertice becomes active or inactive. If only one of the logical
  arguments, say \code{vertex.activity}, is set to \code{FALSE}, then
  only the time points at which edges become active and inactive are
  returned. Similarly for \code{edge.activity = FALSE}. If both are
  \code{FALSE}, numeric(0) is returned.


  For the 'get.vertex.activity' and 'get.edge.activity' functions, a list
  of activity spells is returned.  The activity spells are formatted
  as 2 column matrices; details regarding the cells of the matrices can
  be found in the 'activity.attribute' page.
}

%\references{ ~put references to the literature/web site here ~ }
\author{ Ayn Leslie-Cook \email{aynlc3@uw.edu}, Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{activity.attribute}} }
\examples{
  library(networkDynamic)
  data(flo)
  net1 <- network(flo)
  activate.edges(net1, onset=1:20, terminus=101:120)
  activate.vertices(net1, at=seq(2,32,2))
  get.change.times(net1)
  get.edge.activity(net1)
  get.vertex.activity(net1)
  
  a <-network.initialize(5)
  activate.vertices(a,onset=0,terminus=Inf)
  get.change.times(a,ignore.inf=FALSE)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
