\name{network.extract}
\alias{network.extract}
\alias{\%t\%}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Temporal Extracts/Cross-sections of Dynamically Extended Network Objects }
\description{
  Given a specified time point (or interval), return a reduced network
  at the given point or over the given interval.
}
\usage{
network.extract(x, onset = NULL, terminus = NULL, length = NULL, at = NULL,
    rule = c("any", "all"), active.default = TRUE,retain.all.vertices = FALSE)

x \%t\% at
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{\link[network]{network}}. }
  \item{onset}{ optionally, the start of the specified interval.  This must be  accompanied by one of \code{terminus} or \code{length}.}
  \item{terminus}{ optionally, the end of the specified interval.  This must be  accompanied by one of \code{onset} or \code{length}.}
  \item{length}{ optionally, the length of the specified interval. This must be  accompanied by one of \code{onset} or \code{terminus}.}
  \item{at}{ optionally, a single time point.}
  \item{rule}{ a text string for defining \dQuote{active} for this call:  \code{any} if elements active at any time during the interval are to be used, or \code{all} if elements must be active over the entire interval. }
  \item{active.default}{ logical; should elements without an activity attribute be regarded as active by default? }
  \item{retain.all.vertices}{ logical; should the extracted network retain all vertices, ignoring the vertex activity spells of x in order to ensure that the network returned has the same size as x?}
}
\details{
  For the purposes of extraction, edges are only considered active over
  some interval if: the edge itself is active over this
  time period, and both of the incident vertices are also active over the given time period.
  
  When \code{retain.all.vertices} is false (the default), the function has the following behavior:
 \itemize{
	 \item If \code{at} is used to specify the spell of interest, the returned network consists of all edges and vertices
  active at that timepoint.  
	\item If an interval is supplied to specify the spell (via \code{onset} and one of \code{terminus} or \code{length}), edges and vertices active
  over the specified interval are used.   If \code{rule=="any"}, then edges and vertices active at any time during the interval are returned; otherwise, only those active during the entire period are returned. 
 }

  Note that only active vertices are included by default.  As a result, the size of
  the extracted network may be smaller than the original, and vertex and
  edge ids will be translated, but may not correspond to their original values.  To
  preserve network size, use \code{retain.all.vertices == TRUE}.  Under this rule, all vertices are preserved,
  and edges that are active (by the specified \code{rule}) during the specified spell are included in the returned network.
  
  \code{\%t\%} (the temporal cross-section operator) is  a
  simplified (and less flexible) wrapper for \code{network.extract} that
  returns the network of active vertices and edges at a given time point.
}
\value{
  A \code{network} object containing the active edges and vertices for
  the specified spell, when \code{retain.all.vertices==FALSE},
  otherwise the network object containing all vertices, and only those edges
  active for the specified spell.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Carter T. Butts \email{buttsc@uci.edu}}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{is.active}}, \code{\link{activity.attribute}}, \code{\link{network.extensions}} }
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
