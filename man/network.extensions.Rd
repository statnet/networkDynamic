\name{network.extensions}
\alias{network.extensions}
\alias{get.edgeIDs.active}
\alias{get.edges.active}
\alias{get.neighborhood.active}
\alias{is.adjacent.active}
\alias{network.dyadcount.active}
\alias{network.edgecount.active}
\alias{network.naedgecount.active}
\alias{network.size.active}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Dynamically Extended Core Network Functions }
\description{
  Various core functions from the \link[network]{network} package, with specialized extensions for handling dynamic data.
}
\usage{
get.edgeIDs.active(x, v, onset, terminus = NULL, alter = NULL,
    neighborhood = c("out", "in", "combined"), rule = c("any", "all"),
    na.omit = TRUE, active.default = TRUE)
get.edges.active(x, v, onset, terminus = NULL, alter = NULL,
    neighborhood = c("out", "in", "combined"), rule = c("any", "all"),
    na.omit = TRUE, active.default = TRUE)

get.neighborhood.active(x, v, onset, terminus = NULL, 
    type = c("out", "in", "combined"), rule = c("any", "all"), 
    na.omit = TRUE, active.default = TRUE)

is.adjacent.active(x, vi, vj, onset, terminus = NULL, 
    rule = c("any", "all"), na.omit = FALSE, active.default = TRUE)

network.dyadcount.active(x, onset, terminus = NULL, 
    rule = c("any", "all"), na.omit = TRUE, active.default = TRUE)
network.edgecount.active(x, onset, terminus = NULL, 
    rule = c("any", "all"), na.omit = TRUE, active.default = TRUE)
network.naedgecount.active(x, onset, terminus = NULL, 
    rule = c("any", "all"), active.default = TRUE)
network.size.active(x, onset, terminus = NULL, rule = c("any", "all"),
    active.default = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{\link[network]{network}}}
  \item{v}{ a vertex ID. }
  \item{vi}{a vertex ID (tail).}
  \item{vj}{a vertex ID (head).}
  \item{onset}{ an onset time for selecting edges/vertices. }
  \item{terminus}{ an optional termination time for the sampling interval. }
  \item{alter}{ optionally, the ID of another vertex. }
  \item{neighborhood}{ an indicator for whether we are interested in in-edges, out-edges, or both (relative to \code{v}).  }
  \item{type}{ as \code{neighborhood}, above, for vertex neighborhoods. }
  \item{rule}{ \code{any} to sample elements appearing at any time during the sampling interval, or \code{all} to sample only elements active for the entire interval. }
  \item{na.omit}{ logical; should we omit missing edges? }
  \item{active.default}{ logical; should elements without an activity attribute be regarded as active by default? }
}
\details{
  These functions are \dQuote{dynamically extended} versions of their standard \link[network]{network} counterparts.  As such, conventional aspects of their behavior can be found on their respective manual pages (see below).  In all cases, the sole difference between the extended and non-extended versions lies in the ability of the user to specify a time point (or interval), such that only vertices and edges active at the indicated time (or during the indicated interval) are employed by the routine.
  
  Temporal selection for these routines is governed by the \code{onset} argument, along with the optional \code{terminus}.  When only an onset time is provided, then a temporal cross-section at the specified time point is used; when both onset and termination times are given, elements active during the corresponding interval are employed.  In this latter case, the \code{rule} argument determines whether all elements active at any time during the interval are used (\code{"any"}), or whether only those active during the entire interval (\code{"all"}) should be employed.  For elements (edges and vertices) lacking an activity attribute, the \code{active.default} argument determines appropriate treatment.  By default, all such elements are considered active.
}
\value{
  The return value matches that of the original function, suitably filtered.
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/} }
\author{ Carter T. Butts \email{buttsc@uci.edu} }
\note{ The current implementation of these routines may be quite slow compared to the originals.  Do not use them unless you have to. }
\seealso{ \code{\link{network.crosssection}}, \code{\link{is.active}}, \code{\link[network]{get.edgeIDs}}, \code{\link[network]{get.edges}}, \code{\link[network]{get.neighborhood}}, \code{\link[network]{is.adjacent}}, \code{\link[network]{network.dyadcount}}, \code{\link[network]{nework.edgecount}}, \code{\link[network]{network.naedgecount}}, \code{\link[network]{network.size}}}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
