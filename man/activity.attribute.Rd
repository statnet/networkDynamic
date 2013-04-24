\name{activity.attribute}
\alias{activity.attribute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Activity Attributes for Dynamically Extended Networks }
\description{
  Dynamically extended networks are \link[network]{network} class objects whose vertex and edge components bear timing information.  This page describes the nature of this information in the current incarnation of this package.
}
%\usage{
%}
%- maybe also 'usage' for other objects documented here.
%\arguments{
%  \item{x}{ ~~Describe \code{x} here~~ }
%}
\details{
  Currently, each edge and vertex in a dynamically extended network is presumed to be in one of two states (\dQuote{active} or \dQuote{inactive}) at any given point in time.  The state of a network element (i.e., edge or vertex) is governed by an attribute with the name \dQuote{active}, which is considered a reserved term for purposes of this package.
  
  The activity attribute consists of a two-column numeric matrix, each row of which contains an activity spell, and the two columns of which encode onset and terminus times (respectively).  Elements are presumed inactive until the onset of their first activity spell, and are likewise presumed inactive after the termination of their last spell; spells must be consecutive and strictly non-overlapping, and are taken to span the period from the onset (inclusive) to the terminus (exclusive).  Any numeric values may be used for spell timing, including \code{Inf} and \code{-Inf}.  (The latter are used to create spells whch are open-ended.)
  
  Elements which are strictly inactive (i.e., with no valid activity spells) may be denoted by a matrix considing only of the special \dQuote{null} spell, \code{c(Inf,Inf)}.  Functions using this package extensions should interpret this spell to indicate complete inactivity.  This spell is likewise incompatible with other spells, and should be replaced whenever an activation enters the element's event history.

  Although various special functions are provided to aid access to the \code{active} attribute, they are not strictly necessary: any standard \link[network]{network} methods (e.g., \code{\link[network]{get.edge.attribute}}) may be used to get or set this information.  Care should be taken, however, to preserve the mandatory two-column structure when setting or modifying activity attributes.
}
%\value{
%  ~Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
%  ...
%}
%\references{ ~put references to the literature/web site here ~ }
\author{ Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning 5}{....} ~
%}
\seealso{ \code{\link{is.active}}, \code{\link{activate}} }
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
