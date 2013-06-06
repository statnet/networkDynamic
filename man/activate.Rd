%  File networkDynamic/man/activate.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{activate}
\alias{activate}
\alias{activate.edges}
\alias{activate.vertices}
\alias{deactivate.edges}
\alias{deactivate.vertices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Dynamic Activation and Inactivation of Edges and Vertices }
\description{
  \code{activate} and \code{deactivate} set spells of activity and inactivity respectively for elements (edges and vertices) in a dynamic network.  
}
\usage{
activate.edges(x, onset = NULL, terminus = NULL, length = NULL, at = NULL,
    e = seq_along(x$mel))
activate.vertices(x, onset = NULL, terminus = NULL, length = NULL, at = NULL, 
    v = seq_len(network.size(x)))

deactivate.edges(x, onset = NULL, terminus = NULL, length = NULL, at = NULL,
    e = seq_along(x$mel))
deactivate.vertices(x, onset = NULL, terminus = NULL, length = NULL, at = NULL,
    v = seq_len(network.size(x)), deactivate.edges = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{\link[network]{network}}. }
  \item{onset}{ an optional vector of timepoints that specifies the starts of the interval(s). This must be accompanied by one of \code{terminus} or \code{length}. }
  \item{terminus}{ an optional vector of timepoints that specifies the ends of the interval(s). This must be accompanied by one of \code{onset} or \code{length}. }
  \item{length}{ an optional vector of interval lengths for the interval(s). This must be accompanied by one of \code{onset} or \code{terminus}. }
  \item{at}{ optional, one or more time points to be activated. }
  \item{e}{ optional, one or more IDs indicating edges to be modified. Ids can be repeated to indicate multiple time values per element.}
  \item{v}{ optional, one or more IDs indicating vertices to be modified. Ids can be repeated to indicate multiple time values per element.}
  \item{deactivate.edges}{optional, TRUE setting will automatically deactivate all the edges associated with the vertex being deactivated}
}
\details{
  An element in a dynamically extended network class is considered to be either \dQuote{active} or \dQuote{inactive} at any point in time, with the substantive meaning of \dQuote{activity} determined by the specific application context.  The \code{activate} and \code{deactivate} functions provide an interface for controlling the state of the elements in a dynamic network.
  
  \code{activate.edges} and \code{activate.vertices} have identical behavior, except for the elements they modify (the same is true for the \code{deactivate.*} functions).  
  
There are several ways to specify the activity spell, and the general syntax rules are described at \code{\link{activity.attribute}}.  Activity can be set for a single time point, using either \code{at}, or by setting onset=terminus.  Activity can be set for an interval [onset,terminus), using a valid combination of the \code{onset}, \code{terminus} and \code{length} attributes.

  
This allows for a wide range of options when specifying spells, but a correct specification must use only one of these forms:
\preformatted{
  at
  onset and terminus 
  onset and length
  terminus and length
  or, you can provide no timing information 
}

If provided with no timing information, the element is activated/deactivated from \code{-Inf} to \code{Inf}.   
The specified interval spans the period from the onset (inclusive) to the terminus (exclusive), so [onset,terminus).


There are some special behaviors associated with the arguments  \code{Inf} and \code{-Inf}.
\itemize{
	\item The open-ended interval \code{c(-Inf,x)} includes \code{-Inf}.  For consistency, we also allow the open-ended interval \code{c(x, Inf)} to include \code{Inf}. 
Thus [onset, terminus) will be interpreted as [onset, terminus] when terminus = \code{Inf}.
	\item The arguments  \code{Inf} or \code{-Inf} are only valid when used to specify an interval, they can not be used 
to specify status at a time point using  \code{at}.   In addition, they cannot be paired with themselves in a call.
That is, both \code{(Inf,Inf)} and \code{(-Inf,-Inf)} are not valid specifications for any spell.
	\item Both \code{deactivate.*(x)} and \code{deactivate.*(x, -Inf, Inf)} create the null spell -- specifying inactivity over the entire time span. Note that
by convention the null spell is stored as \code{(Inf,Inf)}.
}
   
Calling the activation/deactivation functions with a vector of spell modifiers and a vector of elements to be modified
(for example, \code{at=c(1,3,5 7), v=c(1:4)}) allows multiple elements in the network 
to be activated/deactivated simultaneously (note, not multiple spells for a single element).  
The spell modifiers are applied sequentially to the selected elements.   
If the length of the spell vector is less than the number of elements, 
the spell modifiers are recycled as needed.  
When multiple network elements are activated in a single call, the spell modifiers must all be of one type, 
either \code{at}, or a valid mix of \code{onset}, \code{terminus} and \code{length}.

The \code{activate.*} and \code{deactivate.*} functions in general modify spells in similar, if opposite, ways.  
However, there are some behaviors that are specific to each function.

\itemize{ 
	\item Effects on an element that has no existing activity attributes.
	\itemize{
		\item For activate: the element is marked as being inactive before the onset time, then active for the specified spell, then inactive from the terminus on.  
		\item For deactivate: the element is marked as being active before the onset time, then inactive for the specified spell, then active from the terminus on (i.e., the opposite of \code{activate.*}.
	}
	\item Effects of the \code{at}  specification.
	\itemize{
		\item For activate: the element is activated at that time point.  
		\item For deactivate: the element is deactivated at the  time point, but only if the time point is currently activated as a 0-length spell.
	}
}

  Currently, there is no support for activating multiple spells for a single element
  in a single call. To activate 2 spells for a node,  for example, one must call
  activate.vertices twice.  It is advisable to remove duplicate edges or
  vertices from the \code{e} or \code{v} input vectors.
  
Edge/vertex activity is tracked through an attribute called (eponymously) \dQuote{active}, and which is explained in more detail under \link{activity.attribute}.  This may be modified or otherwise accessed using standard class methods (e.g., \code{\link[network]{get.edge.attribute}}), as well as the specialized methods described here.
  

}
\value{
  None.  (But all four commands modify their arguments as a side-effect.)
}
\references{ ~put references to the literature/web site here ~ }
\author{ Ayn Leslie-Cook \email{aynlc3@uw.edu}, Carter T. Butts \email{buttsc@uci.edu}}
\note{ This function may not be entirely stable in the long run, since it makes explicit reference to internal elements of the network object; some syntactic changes could occur in the future. }

\seealso{ \code{\link{is.active}} }
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
               
deactivate.edges(triangle, at=2, e=1) # does not work since the spell is not 0-length
is.active(triangle, at=2, e=1:3)

deactivate.edges(triangle, e=1, onset=2, length=0.1)  # this deactivates the vertex
is.active(triangle, at=2, e=1:3)  

# this activates edges 2 and 3 at time 5
activate.edges(triangle, e=2:3, at=5)

# this activates edge 1 at multiple times;
deactivate.edges(triangle)
activate.edges(triangle, e=c(1,1,1), at=6:8)
is.active(triangle, at=6, e=1:3)
is.active(triangle, at=7, e=1:3)
is.active(triangle, at=8, e=1:3)

# this activates vertex 1 for two spells, (0,1) and (3,4)
test <- network.initialize(3)
activate.vertices(test,onset=0:3,terminus=1:4,v=c(1,2,3,1))
get.vertex.activity(test, as.spellList=TRUE)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

