%  File networkDynamic/man/activity.attribute.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{activity.attribute}
\alias{activity.attribute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Activity Attributes for Dynamically Extended Networks }
\description{
  Dynamically extended networks are \code{networkDynamic} class objects (extensions of \link[network]{network} objects) whose vertex and edge elements include timing information. This page describes the types of timing information currently available, and the general syntax for setting or querying this information.
}
%\usage{
%}
%- maybe also 'usage' for other objects documented here.
%\arguments{
%  \item{x}{ ~~Describe \code{x} here~~ }
%}
\details{
  Currently, each edge and vertex in a dynamically extended network is presumed to be in one of two states at any given point in time:  (\dQuote{active} or \dQuote{inactive}).  The state of a network element (i.e., edge or vertex) is governed by an attribute with the name \dQuote{active}, which is considered a reserved term for purposes of this package.
  
  The activity attribute consists of a two-column numeric matrix.  Each
  row contains an activity spell, and the two columns 
  encode onset and terminus times respectively. Elements are presumed
  inactive until the onset of their first activity spell, and are
  likewise presumed inactive after the termination of their last active spell.
  
  Spells are taken to span the period from the onset
  (inclusive) to the terminus (exclusive), so [onset,terminus). 
  They must be consecutive, ordered forward in time, and strictly
  non-overlapping.  
  
  The syntax for defining or querying spells can specify either an instantaneous time point or an interval.  The commands for this include \code{at=},  \code{onset=}, \code{terminus=} and \code{length=}.  Any numeric values may be used in the interval specifications, including  \code{Inf} and \code{-Inf}, (with some restrictions, see below).  A spell with \code{onset=-Inf} represents onset censoring. A spell with \code{terminus=Inf} represents terminus censoring. \code{Inf} and \code{-Inf} cannot be used with the \code{at} specification. Similarly, \code{onset} can not be \code{Inf} and \code{terminus} can not be {-Inf}.
  
  The general syntax rules for specifying spells are as follows:
    
    To specify a spell as a single time point:
    \itemize{
    	\item use the \code{at} argument, or
    	\item use \code{onset=terminus}.
    }
    To specify a spell as a particular interval, one of the following combinations of \code{onset}, \code{terminus} and \code{length} is required:
    \itemize{
  	  \item \code{onset} and \code{terminus} 
  	  \item \code{onset} and \code{length} 
  	  \item \code{terminus} and \code{length}  
    }
 The special \dQuote{null} spell is used to designate elements that are never active (i.e., have no valid activity spells).  These can only be set by the \code{deactivate} function (see \code{\link{activity.attribute}}) and by convention are stored as \code{c(Inf,Inf)}.  \dQuote{Null} spells are incompatible with other spells, and should be replaced whenever an activation enters the element's event history.

  Although it is possible to access and modify the activity spells using \link[network]{network} attribute methods (e.g., \code{\link[network]{get.edge.attribute}})  it is not recommended, and extreme care should be taken to preserve the structure of the spell matrix. The preferred way to modify the spells of a network element is with the \code{\link{activate}} related methods. Vertices and edges with specific activity ranges can be selected using \code{\link{network.extensions}}.
  
  In addition, a number of special functions are also provided to simplify common tasks related to the \code{active} attribute (noted below). 
  
}

%\references{ ~put references to the literature/web site here ~ }
\author{ Ayn Leslie-Cook \email{aynlc3@uw.edu}, Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning 5}{....} ~
%}
\seealso{ \code{\link{is.active}}, \code{\link{activate}}, \code{\link{activate.vertices}},\code{\link{activate.edges}}, \code{\link{deactivate.vertices}},\code{\link{deactivate.edges}},\code{\link{network.extensions}}}
\examples{
triangle <- network.initialize(3)  # create a toy network

activate.vertices(triangle,onset=1,terminus=5,v=1) 
activate.vertices(triangle,onset=1,terminus=10,v=2)
activate.vertices(triangle,onset=4,terminus=10,v=3)
deactivate.vertices(triangle,onset=2, length=2, v=1)

get.vertex.activity(triangle) # vertex spells
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
