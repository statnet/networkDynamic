\name{adjust.activity}
\alias{adjust.activity}
\title{
Adjust the activity ranges in all of the spells of a networkDynamic object
}
\description{
Transforms the values of all the activity spells of vertices and edges, as well as spells of dynamic TEA attributes (and the \code{net.obs.period} if it exists).
}
\usage{
adjust.activity(nd, offset = 0, factor = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
  networkDynamic object to be modified
}
  \item{offset}{
numeric value to be added (or subtracted) to all spell values
}
  \item{factor}{
numeric factor for multiplying all spell values
}
}
\details{
Spell values are modified by first adding the value of \code{offset} and then multiplying by \code{factor}. If a \code{\link{net.obs.period}} attribute exists, it's observation spell values are transformed, and the \code{time.increment} value is multiplied by \code{factor}. 
}
\value{
The \code{nd} argument is modified in place and returned invisibly.
}


\author{
skyebend@uw.edu
}


\seealso{
 See also \code{\link{activity.attribute}}, \code{\link{attribute.activity.functions}} and \code{\link{net.obs.period}}
}
\examples{
# convert steps of an imaginary discrete sim
# with each steps corresponding to 1 day
# into decimal 'years' since 1990
sim<-network.initialize(5)
activate.vertices(sim,onset=0,terminus=2546)
add.edges.active(sim,head=1:4,tail=2:5,onset=0,terminus=2546)
set.network.attribute(sim,'net.obs.period',list(
   observations=list(c(0,2546)),mode="discrete", 
   time.increment=1,time.unit="step"))

# do the transformation
adjust.activity(sim,offset=1990*365.25,factor=1/365.25)

# modify the 'units' of net.obs.period
obs <-get.network.attribute(sim,'net.obs.period')
obs$time.unit<-'year'
set.network.attribute(sim,'net.obs.period',obs)

# peek at the new values
as.data.frame(sim)
get.network.attribute(sim,'net.obs.period')
}