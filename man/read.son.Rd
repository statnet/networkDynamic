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
Read .son (SoNIA) dynamic network input files to create a dynamic network object
}
\description{
A .son file is a tab seperated text file with two sections, the first describing node attribute events and the second arc attribute event. Each section has a set of defined column names.  
}
\usage{
read.son(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{path to the file to be read}
}
\details{
For more details about the format, see the SoNIA file specification for more information: \url{http://sourceforge.net/apps/mediawiki/sonia/index.php?title=Son_format}. Uses \code{\link{as.networkDynamic}} internally to build a dynamic network out of an array of vertex spells and an array of edge spells. Does not currenctly support the \code{AlphaId} node index for permitting arbitrary node ids. 
}
\value{
A network dynamic object with vertex and edge durations corresponding the the spells defined in the input file. 
}
\references{
\url{http://sourceforge.net/apps/mediawiki/sonia/index.php?title=Son_format}.
}
\author{
Skye Bender-deMoll
}
\note{
The current version of this function does not support reading the (dynamic) node and arc attributes.
}


\seealso{
See Also \code{\link{as.networkDynamic}}
}
\examples{
# classroom<-read.son(system.file('extdata/cls33_10_16_96.son',package='networkDynamic'))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ graphs }
\keyword{ utilities }
\keyword{ IO }
\keyword{ files }
