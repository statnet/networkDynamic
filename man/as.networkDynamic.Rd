%  File networkDynamic/man/as.networkDynamic.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{as.networkDynamic}
\alias{as.networkDynamic}
\alias{as.networkDynamic.network}
\alias{as.networkDynamic.networkDynamic}

\title{
as.networkDynamic
}
\description{
Functions with these names formerly provided S3-dispatched functions for converting between various dynamic network formats which have been DEPRECATED. Use \code{\link{networkDynamic}} instead. Current application only sets the networkDynamic class on objects. 
}



\usage{
                 
\method{as.networkDynamic}{networkDynamic}(object,...)

\method{as.networkDynamic}{network}(object,...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{a \code{network} or \code{networkDynamic} object}

  \item{\dots}{ignored}

}
\details{
 \code{as.networkDynamic.network} converts a network object into networkDynamic object by adding a \code{networkDynamic} class.
}



\value{
  For \code{as.networkDynamic.network} the input object is returned with a \code{networkDynamic} class added.  For \code{as.networkDynamic.networkDynamic} the input object is returned. 
}

\note{
Function modifies its argument
}

\author{
Pavel, Zack W Almquist <almquist@uci.edu>
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
For previous functionality, see  \code{\link{networkDynamic}}. For the inverse (removing networkDynamic class) see \code{\link{as.network.networkDynamic}}.
}

\examples{
 nd<-as.networkDynamic(network.initialize(3))
 class(nd)
 is.networkDynamic(nd)
}
