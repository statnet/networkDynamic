%  File networkDynamic/man/network.extensions.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{network.extensions}
\alias{network.extensions}
\alias{get.edgeIDs.active}
\alias{get.edges.active}
\alias{get.neighborhood.active}
\alias{is.adjacent.active}
\alias{network.dyadcount.active}
\alias{network.edgecount.active}
\alias{network.naedgecount.active}
\alias{network.size.active}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Dynamically Extended Core Network Functions }
\description{
  Various core functions from the \link{network} package, with specialized extensions for handling dynamic data.
}
\usage{
get.edgeIDs.active(x, v, onset = NULL, terminus = NULL, length = NULL, at = NULL,
    alter = NULL, neighborhood = c("out", "in", "combined"), rule = c("any", "all"), 
    na.omit = TRUE, active.default = TRUE)
get.edges.active(x, v, onset = NULL, terminus = NULL, length = NULL, at = NULL,
    alter = NULL, neighborhood = c("out", "in", "combined"), rule = c("any", "all"),
    na.omit = TRUE, active.default = TRUE)
get.neighborhood.active(x, v, onset = NULL, terminus = NULL, length = NULL, at = NULL,
    type = c("out", "in", "combined"), rule = c("any", "all"), 
    na.omit = TRUE, active.default = TRUE)

is.adjacent.active(x, vi, vj, onset = NULL, terminus = NULL, length = NULL, at = NULL, 
    rule = c("any", "all"), na.omit = FALSE, active.default = TRUE)

network.dyadcount.active(x, onset = NULL, terminus = NULL, length = NULL, at = NULL, 
    rule = c("any", "all"), na.omit = TRUE, active.default = TRUE)
network.edgecount.active(x, onset = NULL, terminus = NULL, length = NULL, at = NULL,
    rule = c("any", "all"), na.omit = TRUE, active.default = TRUE)
network.naedgecount.active(x, onset = NULL, terminus = NULL, length = NULL, at = NULL, 
    rule = c("any", "all"), active.default = TRUE)
network.size.active(x, onset = NULL, terminus = NULL, length = NULL, at = NULL, 
    rule = c("any", "all"), active.default = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{\link[network]{network}}}
  \item{v}{ a required vertex ID. }
  \item{vi}{an optional vertex ID (tail).}
  \item{vj}{an optional vertex ID (head).}
  \item{onset}{ an optional start time for the selection interval. This must be  accompanied by one of \code{terminus} or \code{length}.  }
  \item{terminus}{ an optional end time for the selection interval.  This must be accompanied by one of \code{onset} or \code{length}.  }
  \item{length}{ the length of the selection interval. This must be  accompanied by one of \code{onset} or \code{terminus}}
  \item{at}{ a time point for selecting edges/vertices. }
  \item{alter}{ optionally, the ID of another vertex. }
  \item{neighborhood}{ a text string that indicates whether we are interested in in-edges, out-edges, or both (relative to \code{v}).  }
  \item{type}{ a text string that functions as \code{neighborhood}, above, for vertex neighborhoods. }
  \item{rule}{ a text string for defining \dQuote{active} for this call:  \code{any} (default) to select elements appearing at any time during the specified interval, or \code{all} to select only elements active for the entire interval. }
  \item{na.omit}{ logical; should we omit missing edges? }
  \item{active.default}{ logical; should elements without an activity attribute be regarded as active by default? }
}
\details{
  These functions are \dQuote{dynamically extended} versions of their standard \link[network]{network} counterparts.  As such, conventional aspects of their behavior can be found on their respective manual pages (see below).  In all cases, the sole difference between the extended and non-extended versions lies in the ability of the user to specify a time point (or interval), such that only vertices and edges active at the indicated time (or during the indicated interval) are employed by the routine.
  
%  Temporal selection for these routines is governed by the \code{onset} argument, along with the optional \code{terminus}.  When only an onset time is provided, then a temporal cross-section at the specified time point is used; when both onset and termination times are given, elements active during the corresponding interval are employed.  In this latter case, the \code{rule} argument determines whether all elements active at any time during the interval are used (\code{"any"}), or whether only those active during the entire interval (\code{"all"}) should be employed.
  
For elements (edges and vertices) lacking an activity attribute, the \code{active.default} argument determines appropriate treatment.  By default, all such elements are considered active.
}
\value{
  The return value matches that of the original function, suitably filtered.
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/} }
\author{ Carter T. Butts \email{buttsc@uci.edu} }
\note{ The current temporal implementation of these routines will be slower compared to the non-temporal versions.  Do not use them unless you have to. }
\seealso{ \code{\link{network.extract}}, \code{\link{is.active}}, \code{\link[network]{get.edgeIDs}}, \code{\link[network]{get.edges}}, \code{\link[network]{get.neighborhood}}, \code{\link[network]{is.adjacent}}, \code{\link[network]{network.dyadcount}}, \code{\link[network]{network.edgecount}}, \code{\link[network]{network.naedgecount}}, \code{\link[network]{network.size}}}
\examples{

## is.adjacent.active
triangle <- network.initialize(3)    # create a toy network
add.edge(triangle,1,2)               # add an edge between vertices 1 and 2
add.edge(triangle,2,3)               # add a more edges
add.edge(triangle,3,1)
is.adjacent.active(triangle,vi=1,vj=2,onset=2,length=1)

## get.edges.active
net <-network.initialize(5)
net[1,2]<-1;
net[2,3]<-1;
activate.edges(net,onset=1,terminus=Inf,e=1)
activate.edges(net,onset=2,terminus=3,e=2)
get.edges.active(net,v=2,at=1)
get.edges.active(net,v=2,at=1, neighborhood="combined")
get.edges.active(net,v=2,at=2, neighborhood="combined")

## get the ids of the active edges instead (could be multiple incident edges)
get.edgeIDs.active(net,v=2,at=2)

## or get ids of the active nodes in a vertex neighborhood
get.neighborhood.active(net,v=2,at=2)

# returns both edges
get.edges.active(net,v=2,onset=1, terminus=3, neighborhood="combined", rule='any')
# returns only one edge (e=2)
get.edges.active(net,v=2,onset=1, terminus=3, neighborhood="combined", rule='all')

## network.edgecount.active
network.edgecount.active(net, at=1, active.default=FALSE)
network.edgecount.active(net, at=2, active.default=FALSE)
network.edgecount.active(net, at=5, active.default=FALSE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
