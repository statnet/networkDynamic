%  File networkDynamic/man/read.son.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{read.son}
\alias{read.son}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read .son-formatted (SoNIA) input files containing longitudinal network data and  create a corresponding networkDynamic object
}
\description{
A .son file is a tab-separated text file with two sections, the first describing node attribute events and the second arc attribute events. Each section has a set of defined column names as a header. See the SoNIA file specification for more information: \url{https://sourceforge.net/p/sonia/wiki/Son_format/}.
}
\usage{
read.son(file, guess.TEA=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{path to the file to be read}
  \item{guess.TEA}{logical: should the parser guess that non-changing attribute values are intended to be static instead of TEAs?}
}
\details{
 Uses \code{\link{networkDynamic}} internally to build a dynamic network out of an array of vertex spells and an array of edge spells.  Vertex and edge attributes will be attached, usually as TEA.  However, If the values of an attribute never change, they will be attached as a static attribute.  This is convenient but will be incorrect if a value was intended to be defined for only a single spell matching with the vertex activation. Setting \code{guess.TEA=FALSE} will force all attributes to be loaded as TEAs (slower).
}
\value{
A network dynamic object with vertex and edge durations and attributes corresponding the the spells defined in the input file. 
}
\references{
\url{https://sourceforge.net/p/sonia/wiki/Son_format/}.
}
\author{
Skye Bender-deMoll
}
\note{
The current version of this function does not support reading clusters.
}


\seealso{
See Also \code{\link{networkDynamic}}
}
\examples{
classroom<-read.son(system.file('extdata/cls33_10_16_96.son',package='networkDynamic'))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ graphs }
\keyword{ utilities }
\keyword{ IO }
\keyword{ files }
