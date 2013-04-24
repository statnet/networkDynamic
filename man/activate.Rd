\name{activate}
\alias{activate}
\alias{activate.edges}
\alias{activate.vertices}
\alias{deactivate.edges}
\alias{deactivate.vertices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Dynamic Activation and Inactivation of Edges and Vertices }
\description{
  The \code{activate} and \code{deactivate} respectively set periods of activity and inactivity for edges and vertices in a network with dynamic extensions.  
}
\usage{
activate.edges(x, onset = -Inf, terminus = NULL, 
    e = 1:length(x$mel))
activate.vertices(x, onset = -Inf, terminus = NULL, 
    v = 1:network.size(x))

deactivate.edges(x, onset = Inf, terminus = NULL, 
    e = 1:length(x$mel))
deactivate.vertices(x, onset = Inf, terminus = NULL, 
    v = 1:network.size(x))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{\link[network]{network}}. }
  \item{onset}{ one or more onset times for the desired interval. }
  \item{terminus}{ optionally, one or more termination times for the desired interval. }
  \item{e}{ one or more IDs indicating edges to be modified. }
  \item{v}{ one or more IDs indicating vertices to be modified. }
}
\details{
  An edge or vertex in a dynamically extended network class is considered to be either \dQuote{active} or \dQuote{inactive} at any point in time, with the precise substantive meaning of \dQuote{activity} being application-specific.  The \code{activate} and \code{deactivate} functions provide an interface for controlling the state of such a network's elements.
  
  \code{activate.edges} and \code{activate.vertices} have identical behavior, except for the elements they modify.  Calling either with a single onset argument will set the state of the selected element(s) to \dQuote{active} at the specified time point.  If the element is already active, this has no effect; otherwise, the element is presumed to be active until it reaches its next transition to inactivity (if any).  If called on a element lacking an activity attribute, the element is set to be inactive until the onset time, and active thereafter - compare to the corresponding behavior of the \code{deactivate} functions as described below.  Calling these functions with multiple onsets (i.e., a vector of onset times) allows for the simultaneous adjustment of multiple elements, with the elements of \code{onset} being applied sequentially to the selected elements.
  
  If the optional \code{terminus} argument is supplied, then activation is attempted for the interval from onset to terminus only.  When applied to a element without an activity attribute, the element is set to be active until the onset time, becoming inactive again after terminus.  As with \code{onset}, \code{terminus} may be given as a vector of values.

  The \code{deactivate.edges} and \code{deactivate.vertices} functions behave much as their \code{activate} counterparts, save in that they switch the applicable element state(s) to \dQuote{inactive} at the selected times (or over the selected interval).  Where applied to a element without an activity attribute, \code{deactivate.*} with only an onset argument marks the element as being active until the onset time, and inactive thereafter (i.e., the opposite of \code{activate.*}).  Where both onset and terminus arguments are given, the element is marked as being active until the onset time, and then resuming activation at the terminus.  Naturally, these effects apply only to elements without activity attributes.  

  Edge/vertex activity is tracked through an attribute called (eponymously) \dQuote{active}, and which is explained in more detail under \link{activity.attribute}.  This may be modified or otherwise accessed using standard class methods (e.g., \code{\link[network]{get.edge.attribute}}), as well as the specialized methods described here.
  

}
\value{
  None.  (But all four commands modify their arguments as a side-effect.)
}
\references{ ~put references to the literature/web site here ~ }
\author{ Carter T. Butts \email{buttsc@uci.edu}}
\note{ This function may not be entirely stable in the long run, since it makes explicit reference to internal elements of the network object; some syntactic changes could occur in the future. }

\seealso{ \code{\link{is.active}} }
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
