\name{networkDynamic-package}
\alias{networkDynamic-package}
\alias{networkDynamic}
\docType{package}
\title{
Dynamic Extensions for Network Objects
}
\description{
Simple interface routines to facilitate the handling of network objects with complex intertemporal data.
}
\details{
\tabular{ll}{
Package: \tab networkDynamic\cr
Type: \tab Package\cr
Version: \tab 0.1-1\cr
Date: \tab 2009-04-18\cr
Depends: \tab network\cr
License: \tab GPL-2 or later\cr
LazyLoad: \tab yes\cr
}

The \code{networkDynamic} package provides support for a simple family of dynamic extensions to the \code{\link[network]{network}} class; these are currently accomplished via the standard \code{network} attribute functionality (and hence the resulting objects are still compatible with all conventional routines), but greatly facilitate the practical storage and utilization of dynamic network data. 

The basis for the dynamic extensions is described in \link{activity.attribute}.  Other core routines can be found in the links below.
}
\author{
Carter T. Butts \email{buttsc@uci.edu}

Maintainer: Carter T. Butts \email{buttsc@uci.edu}
}
%\references{
%~~ Literature or other references for background information ~~
%}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%~~ the R documentation directory ~~
\keyword{ package }
\seealso{
\code{\link{activity.attribute}}, \code{\link{activate}}, \code{\link{is.active}}, \code{\link{network.crosssection}}, \code{\link{network.extensions}} \code{\link[network]{network}} 
}
%\examples{
%~~ simple examples of the most important functions ~~
%}
