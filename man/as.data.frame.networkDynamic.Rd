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
Extracts the edge timing information from a networkDynamic object and represent as a data.frame.
}
\description{
Creates a data.frame giving lists of edges and their activity spells corresponding to the networkDynamic object. Edges with \code{-Inf} or \code{Inf} values in the spells may have them censored to an observation window.
}
\usage{
\method{as.data.frame}{networkDynamic}(x, row.names = NULL, optional = FALSE, e =seq_along(x$mel), 
             start = NULL, end = NULL, active.default=TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a networkDynamic object}
  \item{row.names}{ignored}
  \item{optional}{ignored}
  \item{e}{vector of edge ids that should be included in the output.}
  \item{start}{optional, numeric onset-censoring time: time at which the network was first
    observed. Edge spells with onset \code{-Inf} are given this
    as their start time (and marked as left censored). See Details for
    the default values.}
  \item{end}{optional right-censoring time: time at which the network was last
    observed. Edge spells with terminus \code{Inf} are given this
    as their end time (and marked as right censored). See Details for
    the default values.}
  \item{active.default}{logical indicating if edges with no defined activity spells should be considered active by default (and appear as a row in output)}
  \item{\dots}{possible additional arguments}
}
\details{
Exports the edge dynamics of a networkDynamic object as a data.frame giving a table of edges with their activity spells, including whether an activity spell was ``censored'' (onset or termination was not observed within the observation window). 
If a \code{\link{net.obs.period}} attribute exists, the maximal and minimal values will be used as censoring times by default.  Censoring times can also be provided using the \code{start} and \code{end} arguments, and censoring can be prevented by setting \code{start=-Inf, end=Inf}.
}
\value{
A \code{\link{data.frame}} containing timed relational information
describing the networkDynamic object, having the following columns:
\item{onset, terminus}{Onset and terminus times of an edge, respectively, if
  not censored, and left and right censoring times, if censored.}
\item{tail, head}{Tail and head of the edge.}
\item{onset.censored, terminus.censored}{TRUE if onset/terminus time is censored.}
\item{duration}{the duration (terminus-onset) of each spell, evaluated after censoring}
\item{edge.id}{the id of the edge the row corresponds to}
The output \code{data.frame} will be sorted by edge.id, onset,terminus. This means that for most (non-multiplex) networks the group of spells for a specific edge will appear in sequential lines of output, but the overall output is not in a temporal order. 
}

\note{
Vertex and attribute activity is not included in the returned value. If an edge has multiple activity spells, it will have multiple rows in the output. When output in censored, it may not match values returned by \code{\link{get.edge.activity}}
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
as.data.frame(net)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

