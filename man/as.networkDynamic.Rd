%  File networkDynamic/man/as.networkDynamic.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{as.networkDynamic}
\alias{as.networkDynamic}
%\alias{as.networkDynamic.default}
%\alias{as.networkDynamic.network.list}
%\alias{as.networkDynamic.data.frame}
%\alias{as.networkDynamic.list}
\alias{as.networkDynamic.network}
\alias{as.networkDynamic.networkDynamic}

\title{
as.networkDynamic
}
\description{
DEPRECATED S3 class functions for converting between various dynamic network formats and networkDynamic objects and back again. Use \code{\link{networkDynamic}} instead.
}

%\method{as.network.dynamic}{default}(x,...)

%\method{as.networkDynamic}{network.list}(object,...)

%\method{as.networkDynamic}{list}(object,...)

\usage{
% \method{as.networkDynamic}{data.frame}(object, nodeInfo = NULL, n = NULL, directed = TRUE,
%                  hyper = FALSE, loops = FALSE, multiple = FALSE,
%                  bipartite = FALSE, ...)


\method{as.networkDynamic}{network}(object, spells = NULL, toggles = NULL, start =
                 NULL, end = NULL, ...)
                 
\method{as.networkDynamic}{networkDynamic}(object,...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{various. For \code{as.dynamic.data.frame} a dataframe with edge timing information}
  %\item{n}{integer. Network size (number of vertices).}
  %\item{nodeInfo}{data.frame with information on vertex timing. The columns are vertex.id, onset, terminus, NodeId(vertex name)}
  \item{spells}{data.frame with information on vertex timing}
  \item{toggles}{data.frame listing times for edge toggles from starting network. The columns are toggle time, tail vertex id of the edge, and head vertex id of the edge.}
  \item{start}{starting time to be assumed for pre-existing ties when constructing a network from toggles}
  \item{end}{ending time to be assumed for active un-toggled ties when constructing a network from toggles}
  \item{\dots}{ignored right now, possibly additional arguments to control network building}
%  The following items set the parameters of the networkDynamic object being built:
%  \item{n}{See \link{network.initialize}} 
%  \item{directed}{See \link{network.initialize}}
%  \item{hyper}{See \link{network.initialize}}
%  \item{loops}{See \link{network.initialize}} 
%  \item{multiple}{See \link{network.initialize}}
%  \item{bipartite}{See \link{network.initialize}}
}
\details{
This documentation express the basic utility functions for converting between networkDynamic and other forms of dynamic networks. These are a collection of S3 objects and use the S3 dispatch form to decide which as.networkDynamic function to use automatically. Currently conversion for \code{network.list} is for use with the \code{network.list} object in \code{\link[ergm]{ergm}} package. Note these functions are still in development and are quite fragile. 

\itemize{
\item \code{as.networkDynamic} Generic converter function, handles dispatching based on class of the object.
%\item \code{as.networkDynamic.list} Converts network.list objects into networkDynamic objects 
\item \code{as.networkDynamic.data.frame} Converts data.frame objects (edge spell list) into networkDynamic objects. The data.frame assumes a 7 column matrix (onset, terminus, tail, head, left.censored, right.censored, duration). The last 3 columns are optional.
%\item \code{as.networkDynamic.list} This assumes either a list of network objects (e.g., the beach data) or two part list %composed of a data.frame object one with vertex information and one with edge information (e.g., Skye's .son format) %[There is a proposal to alter the spec to edge.list and vertex.list where the user may provide just a list of vertices %and time information, just edges list and time information or both]
\item \code{as.networkDynamic.network} This converts a network object into networkDynamic object. User must supply one parameter; either the edge spells (onset, terminus, tail, head, left.censored, right.censored, duration) or toggles (time, tail, head). If the toggles are specified, user can optionally supply the start and end times of the network's dynamic activity. This function is deprecated and the syntax will likely be changed in the next version of networkDynamic.
}


}
\value{
\code{networkDynamic}, \code{\link{network}}, \code{\link{data.frame}}, \code{\link{list}}.
}
%\references{
%
%}
\author{
Pavel, Zack W Almquist <almquist@uci.edu>
}
\note{
\code{as.networkDynamic} and \code{as.networkDynamic.network} functions are deprecated. The name and syntax of these functions will likely change in the next version of networkDynamic.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
See Also \code{\link{networkDynamic}} \code{\link{duration.matrix}}, \code{\link{activity.attribute},}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
