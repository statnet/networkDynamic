%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2013 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{as.network.networkDynamic}
\alias{as.network.networkDynamic}

\title{
 Remove `networkDynamic' class name from networkDynamic object.
}
\description{
Changes the class of a \code{networkDynamic} object to a plain \code{network} object while leaving all attributes (including dynamic attributes) intact.
}
\usage{
 \method{as.network}{networkDynamic}(x, ...)
}

\arguments{
  \item{x}{
 A \code{networkDynamic} object that will have its class name stripped. 
}
  \item{\dots}{
Possible additional arguments
}
}
\details{
 The primary use-case for this method is to force some other S3 method (like \code{simulate} in the \code{tergm} package, plot, or print) to use 'network' instead of 'networkDynamic' in method dispatching.  Dynamic features data structures be left intact but not recognized by corresponding methods, so effectively broken.  
}
\value{
 Returns the original network with the \code{networkDynamic} class name removed but all other attributes unchanged
}

\author{
 Skye Bender-deMoll
}


\seealso{
See Also as \code{\link{network.extract}} for extracting parts of a networkDynamic, and \code{\link{network.collapse}} for extracting part of a networkDynamic as a static network. \code{\link{as.networkDynamic}} for the inverse operation (adding the networkDynamic class to a static network).
}
\examples{
test<-network.initialize(2)
add.edges.active(test,tail=1,head=2,onset=1,terminus=2)
is.networkDynamic(test)
test<-as.network(test)
is.networkDynamic(test)

}