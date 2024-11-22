%  File networkDynamic/man/Newcomb.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{Newcomb}
\title{Newcomb's Fraternity Networks}
\alias{Newcomb}
\alias{newcomb}
\alias{newcomb.rank}
\alias{Newcomb.rank}
\description{
These 14 networks record weekly sociometric preference rankings
from 17 men attending the University of Michigan in the fall
of 1956; Data were collected longitudinally over 15 weeks,
although data from week 9 are missing.

The men were recruited to live in off-campus (fraternity)
housing, rented for them as part of the Michigan Group Study
Project supervised by Theodore Newcomb from 1953 to 1956. All
were incoming transfer students with no prior acquaintance of
one another.

The data set contains two longitudinal networks as \code{network.list}.

\code{newcomb.rank} is a
\code{network.list} object with 14 networks. Each network is complete and the
edge value \code{rank} is the preference of the \eqn{i}th men for the \eqn{j}th
man from \code{1} through \code{16}.  A \code{1} indicates first 
preference, and no ties were allowed.

\code{newcomb} is a \code{network.list} object that has binary
edge values but is similar in structure to \code{newcomb.rank}
and derived from it. Each network has a tie from the \eqn{i}th
men to the \eqn{j}th man if \eqn{i} had a preference for
\eqn{j} of 8 or less. Otherwise there is not tie from \eqn{i}
to \eqn{j}. Note that since these are ranks, the degree of each vertex (and the total number of edges) does not vary over time
}
\usage{data(newcomb)}
\source{\url{https://github.com/bavla/Nets/tree/master/data/Pajek/ucinet#-newcomb-fraternity}}
\references{See the link above. 
Newcomb T. (1961). The acquaintance process. New York: Holt, Reinhard and Winston.\\
Nordlie P. (1958). A longitudinal study of interpersonal attraction in a natural group setting. Unpublished doctoral dissertation, University of Michigan.\\
White H., Boorman S. and Breiger R. (1977). Social structure from multiple networks, I. Blockmodels of roles and positions. American Journal of Sociology, 81, 730-780. 
}
\details{Use \code{data(package="netdata")} to get a full list of networks.}
\seealso{network, sna}
\keyword{datasets}
\section{Licenses and Citation}{
If the source of the data set does not specified otherwise,
this data set is protected by the Creative Commons License
\url{https://creativecommons.org/licenses/by-nc-nd/2.5/}.

When publishing results obtained using this data set the original authors
should be cited.  In addition this package should be cited as:

Mark S. Handcock, David Hunter, Carter T. Butts, Steven M. Goodreau,
and Martina Morris. 2003 
\emph{statnet: An R package for the Statistical Modeling of Social Networks}
\cr
\url{https://statnet.org/}
and the source should be cited as:

Vladimir Batagelj and Andrej Mrvar (2006): \emph{Pajek datasets}
\cr
\url{https://github.com/bavla/Nets/tree/master/data/Pajek/} 
}
\examples{
data(newcomb)
}