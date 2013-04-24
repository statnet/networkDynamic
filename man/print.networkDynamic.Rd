%  File networkDynamic/man/print.networkDynamic.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{print.networkDynamic}
\alias{print.networkDynamic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Provide a view of a networkDynamic object including timing information
}
\description{
Prints out some very basic descriptive stats about the network's dynamics, and the the normal information printed for network objects.
}
\usage{
  
  \method{print}{networkDynamic}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class networkDynamic including dynamic relational information
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
Prints some info about the number of time events in a networkDynamic object, then calls print.network
}
\value{
 Returns the networkDynamic object passed to the function.
}

\author{
Pavel
}
\note{
Only prints out the network information without any dynamic data.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{print.network}}
}
\examples{
  library(networkDynamic)
  data(flo)
  net1 <- network(flo)
  activate.edges(net1, onset=1:20, terminus=101:120)
  activate.vertices(net1, at=seq(2,32,2))
  print(net1)
}

