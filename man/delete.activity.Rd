%  File networkDynamic/man/delete.activity.matrices.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{delete.activity}
\alias{delete.edge.activity}
\alias{delete.vertex.activity}
\title{ Remove Timing Information in a Dynamically Extended Network Object }
\description{
 Removes the activity spells of a given set of vertices or edges.
}
\usage{
delete.edge.activity(x, e=seq_along(x$mel))

delete.vertex.activity(x, v=seq_len(network.size(x)))
}
\arguments{
  \item{x}{ an object, of class \code{network} or \code{networkDynamic}. }
  \item{e}{ the edges of \code{x} that will have their timing information
    deleted; default = all. }
  \item{v}{ the vertices of \code{x} that will have their timing information
    deleted; default = all. }
}
\details{
  Though the timing information of the edges and/or vertices may be
  removed, other networkDynamic methods will assume activity or
  inactivity across all time points, based on the argument
  \code{active.default}. 
}
\value{
  An object with the same class as \code{x}, equivalent to \code{x} without the
  activity spells of the edges \code{e} or the vertices \code{v}.  
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Ayn Leslie-Cook \email{aynlc3@uw.edu}, Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{activate}} }
\examples{
  library(networkDynamic)
  data(flo)
  net1 <- network(flo)
  activate.edges(net1)
  activate.vertices(net1)
  net2 <- net1
  delete.edge.activity(net1, e=seq(2,40,2))
  delete.edge.activity(net2)
  delete.vertex.activity(net2)
  is.active(net1, at=0, e=c(1,2), active.default=FALSE)
  is.active(net1, at=0, e=c(1,2), active.default=TRUE)
  is.active(net2, at=0, e=1:16, active.default=FALSE)
  is.active(net2, at=0, e=1:16, active.default=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
