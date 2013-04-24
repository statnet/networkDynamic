\name{network.dynamic.check}
\alias{network.dynamic.check}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Verify a Dynamically Extended Network Object }
\description{
  Checks a network object to ensure that any activity information is correctly specified.
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
   \item{Endpoints of active edges (if any) are checked to ensure that they are in fact active during the specified period(s).}
   }
 The IDs vertices or edges failing any of these checks are returned.
}
\value{
  A list with elements
  \item{problem.vertexIDs }{IDs of vertices with incorrect activity settings.}
  \item{problem.edgeIDs }{IDs of edges with incorrect activity settings.}
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{activity.attribute}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
