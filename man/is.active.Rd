%  File networkDynic/man/is.active.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{is.active}
\alias{is.active}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Determine Activation State of Edges and Vertices }
\description{
  Query the activity state of one or more network elements (edges or vertices) for either a single time point or an extended interval.
}
\usage{
is.active(x, onset = NULL, terminus = NULL, length = NULL, at = NULL, e = NULL, v = NULL, 
    rule = c("any", "all"), active.default = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{\link[network]{network}}. }
  \item{onset}{ an optional vector of timepoints that specifies the start of the queried interval(s). This must be accompanied by one of \code{terminus} or \code{length}. }
  \item{terminus}{ an optional vector of timepoints that specifies the end of the queried interval(s). This must be accompanied by one of \code{onset} or \code{length}. }
  \item{length}{ an optional vector of interval lengths for the queried interval(s). This must be
    accompanied by one of \code{onset} or \code{terminus}. }
  \item{at}{ an optional vector of single time point(s) to query. }
  \item{e}{ a vector of edge IDs to evaluate. }
  \item{v}{ a vector of vertex IDs to evaluate. }
  \item{rule}{ a character string indicating the definition of "active" for this query:  \code{any} (default) to define active as any active spell during the interval, or \code{all} to define active as being active over the entire interval.  }
  \item{active.default}{logical; should elements without an activity attribute be regarded as active by default?  }
}
\details{
This function can be used to query the activity status of an element at single time point, or over an interval.  
The specification of timepoints and intervals is the same as for the \code{\link{activate}} function.  
The general rules are discussed in \code{\link{activity.attribute}}.
  
To query status at a single time point, use one of the following:
\itemize{
	\item the \code{at} argument, or 
	\item set \code{onset=terminus}.
} 
      
To query status over a particular interval, one of the following valid combinations of \code{onset}, \code{terminus} and \code{length} is required:
\itemize{
	\item \code{onset} and \code{terminus}.
	\item \code{onset} and \code{length}.
	\item \code{terminus} and \code{length}.  
	\item or, you can specify no information.
}

If the query is specified with no timing information the spell is defined as \code{(-Inf,Inf)}.
The query interval spans the period from the onset (inclusive) to the terminus (exclusive), so [onset,terminus).

There are some special behaviors associated with the arguments  \code{Inf} and \code{-Inf}.
\itemize{
	\item The open-ended interval \code{c(-Inf,x)} includes \code{-Inf}.  For consistency, we also allow the open-ended interval \code{c(x, Inf)} to include \code{Inf}. 
Thus [onset, terminus) will be interpreted as [onset, terminus] when terminus = \code{Inf}.
	\item Queries for intervals specified by \code{c(Inf, Inf)} or \code{c(-Inf, -Inf)} are ignored.
	}
}
\value{
  A logical vector indicating the activity states.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Ayn Leslie-Cook \email{aynlc3@uw.edu}, Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{activity.attribute}}, \code{\link{activate}} }
\examples{
triangle <- network.initialize(3)  # create a toy network
add.edge(triangle,1,2)    # add an edge between vertices 1 and 2
add.edge(triangle,2,3)    # add more edges
add.edge(triangle,3,1)

# turn on all edges at time 1 only (0 length spell)
activate.edges(triangle,at=1) 
# activate edge (1,2) from t=2 to t=3
activate.edges(triangle,onset=2, terminus=3,
               e=get.edgeIDs(triangle,v=1,alter=2))
# activate edge (2,3) from t=4 for 2 time lengths
activate.edges(triangle,onset=4, length=2, 
               e=get.edgeIDs(triangle,v=2,alter=3))

# are the edges active at a particular time?
is.active(triangle, at=1, e=1:3)
is.active(triangle, at=4, e=1:3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
