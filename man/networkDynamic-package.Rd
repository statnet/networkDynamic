%  File networkDynamic/man/networkDynamic-package.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{networkDynamic-package}
\alias{networkDynamic-package}
\docType{package}
\title{
Dynamic Extensions for Network Objects
}
\description{
Simple interface routines to facilitate the handling of dynamic network objects with different types of temporal data.  This allows the user to create, store and query networks that change over time.  Changes include edges that form and dissolve over time, and vertices that enter or leave the network.  In this initial release there is no support for temporally changing attributes (of either vertices or edges); that will be included in an update soon.
}
\details{
\tabular{ll}{
Package: \tab networkDynamic\cr
Type: \tab Package\cr
Version: \tab 0.5\cr
Date: \tab 2013-08-29\cr
Depends: \tab network\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}

The \code{networkDynamic} package provides support for a simple family of dynamic extensions to the \code{\link[network]{network}} class; these employ the standard \code{network} attribute functionality (and hence the resulting objects are still compatible with all conventional routines), but greatly facilitate the practical storage, manipulation and query of dynamic network data. 

The basis for the dynamic extensions is described in \link{activity.attribute}.  Other core routines can be found in the links below.
}
\author{
Carter T. Butts \email{buttsc@uci.edu}, Ayn Leslie-Cook \email{aynlc3@uw.edu}, Zack Almquist \email{almquist@uci.edu}, Skye Bender-deMoll {skyebend@uw.edu}, Martina Morris

Maintainer: Skye Bender-deMoll {skyebend@uw.edu}, Carter T. Butts \email{buttsc@uci.edu}
}
%\references{
%~~ Literature or other references for background information ~~
%}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%~~ the R documentation directory ~~
\keyword{ package }
\seealso{
\code{\link{activity.attribute}}, \code{\link{activate}}, \code{\link{is.active}}, \code{\link{network.extract}}, \code{\link{network.extensions}} \code{\link[network]{network}} 
}
%\examples{
%~~ simple examples of the most important functions ~~
%}
