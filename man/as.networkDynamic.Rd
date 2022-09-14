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
\alias{as.networkDynamic.networkLite}

\title{
as.networkDynamic
}

\description{
The \code{as.networkDynamic} generic provides a very basic conversion to 
\code{networkDynamic} from other network types. It is generally recommended 
to use the \code{\link{networkDynamic}} function instead of 
\code{as.networkDynamic}, because \code{\link{networkDynamic}} provides a 
number of additional features for handling temporal information.
}

\usage{
                 
\method{as.networkDynamic}{networkDynamic}(object,...)

\method{as.networkDynamic}{network}(object,...)

\method{as.networkDynamic}{networkLite}(object,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{a \code{network}, \code{networkLite}, or \code{networkDynamic} object}

  \item{\dots}{ignored}

}
\details{
 \code{as.networkDynamic.network} converts a \code{network} object into 
 a \code{networkDynamic} object by adding a \code{networkDynamic} class.
 
 \code{as.networkDynamic.networkLite} converts a \code{networkLite} object into 
 a \code{networkDynamic} object by first converting it to a \code{network} 
 object and then adding a \code{networkDynamic} class. (\code{networkLite}s are
 a backend data structure used in some \code{EpiModel} and \code{statnet}
 packages, and are not typically handled directly by the end user.)

 Such conversions between network types are used when starting a dynamic 
 simulation from a cross-sectional network and returning the simulation history 
 as a dynamic network, as done in the \code{tergm} package for example.
}

\value{
  For \code{as.networkDynamic.network} the input object is returned with a 
  \code{networkDynamic} class added. For \code{as.networkDynamic.networkLite}, 
  the input object is converted to a \code{network} object and then the 
  \code{networkDynamic} class is added. For 
  \code{as.networkDynamic.networkDynamic} the input object is returned 
  unchanged.
}

\note{
The \code{network} method modifies its argument.
}

\author{
Pavel, Zack W Almquist <almquist@uci.edu>
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
For the inverse (removing the \code{networkDynamic} class) see
\code{\link{as.network.networkDynamic}} and
\code{\link{as.networkLite.network}} (which applies to \code{networkDynamic}s).
For extracting cross-sectional information from a \code{networkDynamic} (which
is often more appropriate than simply removing the \code{networkDynamic}
class), see \code{\link{network.collapse}} and \code{\link{network.extract}}.
For more general construction of \code{networkDynamic}s, see
\code{\link{networkDynamic}}. 
}

\examples{
 nd <- as.networkDynamic(network.initialize(3))
 class(nd)
 is.networkDynamic(nd)
 
 nwL <- networkLite::networkLite(3)
 nwD <- as.networkDynamic(nwL)
 class(nwD)
 is.networkDynamic(nwD)
}
