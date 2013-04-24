\name{network.dynamic.check}
\alias{network.dynamic.check}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Verify a Dynamically Extended Network Object }
\description{
  Checks a networkDynamic object for any activity information that is not correctly specified.
}
\usage{
network.dynamic.check(x, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{network}. }
  \item{verbose}{ logical; should output messages be printed to the user? }
}
\details{
 Currently, \code{network.dynamic.check} runs three types of checks on an input object:
   \enumerate{
   \item{Vertex spell matrices (if any) are checked for legality;}
   \item{Edge spell matrices (if any) are checked for legality;}
   \item{Incident vertices of active edges (if any) are checked to ensure that they are also active during the specified period(s).}
   }
 The IDs of vertices or edges failing any of these checks are returned.
}
\value{
  A list with elements
  \item{problem.vertexIDs }{IDs of vertices with incorrect activity settings.}
  \item{problem.edgeIDs }{IDs of edges with incorrect activity settings.}
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Ayn Leslie-Cook \email{aynlc3@uw.edu}, Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{activity.attribute}} }
\examples{
  test <-network.initialize(2) # make a network
  activate.vertices(test,onset=2,length=5,v=1) # only activate one vertex
  test[1,2] <-1 # add an edge between the verticies
  activate.edges(test,onset=0,terminus=7, e=1) # activate the edge for a spell not including vertex spell
  network.dynamic.check(test) # find the bad edge
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
