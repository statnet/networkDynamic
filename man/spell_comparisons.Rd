%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2013 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{spells.overlap}
\alias{spells.overlap}
\alias{spells.hit}
\alias{search.spell}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions to compare and search spell intervals
}
\description{
spells.overlap checks if two spells overlap at all, spells.hit searches a spell matrix for an overlapping spell
}
\usage{
spells.overlap(s1, s2)

spells.hit(needle, haystack)

search.spell(needle, haystack)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s1}{
First spell for in the comparison, must be a two-element numeric vector in the form [onset,terminus]
}
  \item{s2}{
Second spell for in the comparison, must be a two-element numeric vector in the form [onset,terminus]
}

 \item{needle}{
  The query spell used in the search, must be a two-element numeric vector in the form [onset,terminus] 
 }
 
 \item{haystack}{
  The spell matrix to be searched by \code{needle}. Must be two column numeric matrix meeting the order specifications of a spell matrix. See \link{activity.attribute}
 }
}
\details{
Spell overlap is defined as true if one of the following conditions sets is met, otherwise false:

onset1 >= onset2 AND onset1 < terminus2

terminus1 > onset2 AND terminus1 <= terminus2

onset1 <= onset2 AND terminus1 >= terminus2

}
\value{
\code{spells.overlap} returns a boolean indicating if the two spells overlap or not.  

\code{spells.hit} returns the integer row index of the first (earliest) spell in \code{haystack} that overlaps with \code{needle}, or -1 if no overlaps are found

\code{search.spell} returns a vector containing the row indices of spells in haystack that overlap with needle, or numeric(0) if none found
}

\author{
skyebend@uw.edu
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also \code{\link{activity.attribute}}
}
\examples{
a <- c(1,3)
b <- c(2,5.5)
c <- c(-1,10)
d <- c(4,4)
spells.overlap(a,b)
spells.overlap(b,c)
spells.overlap(a,d)

spellmat <- rbind(c(0,1), c(1,2), c(2,3))
spells.hit(c(1,2),spellmat)

}

