%  File networkDynamic/man/as.data.frame.networkDynamic.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{as.data.frame.networkDynamic}
\alias{as.data.frame.networkDynamic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a networkDynamic object into alternate representations.
}
\description{
Converts a networkDynamic object into a data frame giving lists of edges and their activity spells.
}
\usage{
\method{as.data.frame}{networkDynamic}(x, row.names = NULL, optional = FALSE,
                                       start = NULL, end = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a networkDynamic object}
  \item{row.names}{ignored}
  \item{optional}{ignored}
  \item{start}{Left-censoring time: time at which the network was first
    observed. Edge spells with onset \code{-Inf} are given this
    as their start time (and marked as left censored). See Details for
    the default values.}
  \item{end}{Right-censoring time: time at which the network was last
    observed. Edge spells with terminus \code{Inf} are given this
    as their end time (and marked as right censored). See Details for
    the default values.}
  \item{\dots}{possible additional arguments}
}
\details{
Represents the edge dynamics of a networkDynamic object as a data frame giving lists of edges and their activity spells, including whether an activity spell was
"censored". If \code{x} has \code{attr}-style attribute \code{"start"},
it will be used as the left-censoring time and similarly for
\code{"end"} and right-censoring time.
}
\value{
A \code{\link{data.frame}} containing timed relational information
describing the networkDynamic object, having the following columns:
\item{onset, terminus}{Onset and terminus times of an edge, respectively, if
  not censored, and left and right censoring times, if censored.}
\item{tail, head}{Tail and head of the edge.}
\item{onset.censored, terminus.censored}{TRUE if onset/terminus time is censored.}
\item{duration}{the duration (terminus-onset) of each spell, evaluated after censoring}
}


%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
See Also as \code{\link{get.edge.activity}},\code{\link{get.vertex.activity}}
}

\examples{
net <-network.initialize(3)
net[1,2]<-1;
net[2,3]<-1;
## censoring
activate.edges(net,onset=1,terminus=Inf,e=1)
activate.edges(net,onset=2,terminus=3,e=2)
activate.vertices(net, onset=1, terminus=Inf, v=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

