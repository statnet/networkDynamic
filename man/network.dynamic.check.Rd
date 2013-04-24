%  File networkDynamic/man/network.dynamic.check.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{network.dynamic.check}
\alias{network.dynamic.check}
\alias{is.networkDynamic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Verify a Dynamically Extended Network Object }
\description{
  Checks a networkDynamic object for any activity information that is not correctly specified.
}
\usage{
network.dynamic.check(x, verbose = TRUE, complete = TRUE)
is.networkDynamic(x)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object, hopefully of class \code{networkDynamic}. }
  \item{verbose}{ logical; should output messages be printed to the
    user? }
  \item{complete}{ logical; should a complete test be ran, including
    checks of the activity matrices for edges and vertices, versus a
    check of only edge activity invovling non-active vertices}
}
\details{
  Currently, \code{network.dynamic.check} runs two types of checks on an input object:
  \enumerate{
    \item{If a complete test is run, vertex spell matrices and edge spell
      matrices (if any) are checked for legality, specifically these are
      checked for correct dimensionality and proper spell ordering.}
    \item{If vertex and edge spell matrices are correctly specified or
      assumed to be using \code{complete=FALSE}, incident vertices of
      active edges (if any) are checked to ensure that they are also
      active during the specified period(s).}
  }
  If spell matrices are only acted upon by network or
  networkDynamic methods, there is no need to run a complete test -
  correct dimensionality and spell ordering are guaranteed.
  
  \code{is.networkDynamic} checks if its argument has the class 'networkDynamic'.
}
\value{
  For \code{network.dynamic.check}: A list of logical vectors, one for
  each network component checked: 
  \itemize{
    \item{vertex.checks: }{Whether the spell matrix of each vertex (if any)
      is correctly specified}
    \item{edge.checks: }{Whether the spell matrix of each edge (if any)
      is correctly specified}
    \item{dyad.checks: }{Corresponding to edges, whether the incident vertices
      of each edge are active when the edge is active}
  }
  For \code{is.networkDynamic}: a boolean which is true if the class matches
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
