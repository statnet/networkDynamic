%  File networkDynamic/man/duration.matrix.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{duration.matrix}
\alias{duration.matrix}

\title{ Construct a edge spells list from base network and toggle list. }
\description{
  Given a network object and a toggle list, it will construct a list of edges and activity spells and return that as a data frame. The network object does not need to have edges; it only needs to have enough vertices to cover all the edges specified in the toggle list.
  
  The toggle list should be a three column matrix or data frame, where the first column is the toggle time, the second column is the tail vertex id of the edge, and the third column is the head vertex id of the edge. A toggle represents a switch from active state to inactive, or vice-versa.
  
  
}
\usage{
duration.matrix(nw0, changes, start, end)
}
\arguments{
  \item{nw0}{ an object of class \code{network} }
  \item{changes}{ a matrix or data frame, listing the toggles for edges as specified in the above description }
  \item{start}{ integer, specifies the starting time of the network's dynamic activity }
  \item{end}{ integer, specifies the ending time of the network's dynamic activity }
}
\value{
  Returns a data frame that lists the edge spells specified by the toggles. The columns in the data frame are 
   \code{start end tail head left.censored right.censored duration}. Left and right censoring and duration are calculated
   automatically from the toggle list and the start and end times.
}

%\references{ ~put references to the literature/web site here ~ }
\author{ Ayn Leslie-Cook \email{aynlc3@uw.edu}, Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{as.networkDynamic}} }
\examples{
  library(networkDynamic)
  # duration matrix test
  net <-network.initialize(3)
  net[1,2]<-1;
  net[2,3]<-1;
  net[1,3]<-1;
  class(net)
  # toggle list: time, tail, head
  tog=matrix(c(1,1,2, 1,2,3, 2,1,2, 4,1,3, 4,1,2), ncol=3, byrow=TRUE)
  networkDynamic:::duration.matrix(net, tog, 0, 5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
