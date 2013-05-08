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
get.change.times(x, vertex.activity=TRUE, edge.activity=TRUE, ignore.inf=TRUE,
             vertex.attribute.activity = TRUE, edge.attribute.activity = TRUE,
             network.attribute.activity = TRUE)
get.vertex.activity(x, v=seq_len(network.size(x)), 
                   as.spellList=FALSE, active.default=TRUE)
get.edge.activity(x, e=seq_along(x$mel), as.spellList=FALSE, active.default=TRUE)
}
\arguments{
  \item{x}{ an object, of class \code{network} or \code{networkDynamic}. }
  \item{vertex.activity}{ logical, whether the times at which vertices become
    active or inactive is to be returned. }
  \item{edge.activity}{ logical, whether the times at which edges become
    active or inactive is to be returned. }
  \item{vertex.attribute.activity}{logical, whether the times attached to any dynamic vertex attributes should be returned}
  \item{edge.attribute.activity}{logical, whether the times attached to any dynamic edge attributes should be returned}
  \item{network.attribute.activity}{logical, whether the times attached to any dynamic network attributes should be returned}
  \item{ignore.inf}{logical, whether Inf value will be ignored or not. }
  \item{e}{ IDs for the edges whose activity spells are wanted. }
  \item{v}{ IDs for the vertices whose activity spells are wanted. }
  \item{as.spellList}{should data be returned in spell matrix form? TODO: should this be callsed as.spellmatrix instead?}
  \item{active.default}{logical, should edges / vertices with no spell activity defined be treated as always active (\code{TRUE}) and included in output, or never active (\code{FALSE}) and skipped.}
}
\value{
  For the \code{get.change.times} function, the default settings return a
  vector of all of the finite time points at which any edge or any
  vertex becomes active or inactive, or any of their dynamic attributes change. If only one of the logical
  arguments, say \code{vertex.activity}, is set to \code{FALSE}, then
  the time points for the corresponding parts of the data structure will not be returned.  If both are
  \code{FALSE}, numeric(0) is returned.


  For the \code{get.vertex.activity} and \code{get.edge.activity} functions, a list
  of activity spells is returned with each list element corresponding to the id of the vertex or edge. Deleted edges will appear as NULL entries, as will any spell marked as inactive with the 'null' (Inf,Inf) spell.  If the argument \code{as.spellList=TRUE} the activity spells are instead formatted as a \code{data.frame} and include additional columns to indicate possible onset- and terminus-censoring.  Any 'null' spells and deleted edges will be omited. 
  
  For \code{get.vertex.activity} the columns are \code{[onset,terminus,vertex.id,onset.censored,terminus.censored,duration]}. 
  
  For \code{get.edge.activity} the columns are \code{[onset, terminus, tail, head, onset.censored, terminus.censored, duration, edge.id] }. 
  
  The data.frames will be sorted by id, onset, terminus.
  
  If a \code{net.obs.period} network attribute is set, the range of its \code{$observations} element is substituted in for the -Inf and Inf values that otherwise indicate the onset- and terminus-censored observations.  
}

%\references{ ~put references to the literature/web site here ~ }
\author{ Ayn Leslie-Cook \email{aynlc3@uw.edu}, Carter T. Butts \email{buttsc@uci.edu},skyebend }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ See also \code{\link{activity.attribute}},\code{\link{net.obs.period}},\code{\link{as.data.frame.networkDynamic}} }
\examples{
  library(networkDynamic)
  data(flo)
  net1 <- network(flo)
  activate.edges(net1, onset=1:20, terminus=101:120)
  activate.vertices(net1, at=seq(2,32,2))
  get.change.times(net1)
  get.edge.activity(net1)
  get.edge.activity(net1,as.spellList=TRUE)
  get.vertex.activity(net1)
  
  a <-network.initialize(5)
  activate.vertices(a,onset=0,terminus=Inf)
  get.change.times(a,ignore.inf=FALSE)


}

