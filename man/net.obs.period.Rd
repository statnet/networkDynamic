%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2013 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{net.obs.period}
\alias{net.obs.period}
\docType{data}
\title{
Network attribute describing the observation properties of a networkDynamic object
}
\description{
\code{net.obs.period} is an optional network-level attribute that gives meta-information about the characteristics of the observations, time range and temporal model of a \code{networkDynamic} object.  
}
\format{
  If the attribute is present, it is required to have a specific structure.  It is a 4-element \code{list} with named values.
\itemize{
  \item the \code{observations} element is a list of two-elment spell vectors that give the onset and terminus of the time period(s) during which the network was observed or simulated
  \item A string indicating the temporal model of the network: either \code{discrete} or \code{continuous}
  \item \code{time.increment} is an indicator of the `natural' time unit for iterating over the network (primarily useful for discrete time models)
  \item \code{time.unit} is the name of the unit used by all of the spells in the object: often set to \code{step} for discrete simulations. 

}
}
\details{
Some functions that work with \code{networkDynamic} objects need to know information about the observation design of the network. Is it discrete or continuous time model? When did the observation (or simulation) of the network processes start and end? What are the time units? This information may be stored in the \emph{optional} \code{net.obs.period} network attribute. 

Some examples of possible \code{net.obs.period} values for various networks:
\itemize{
  \item  for an stergm output it might be:
  \code{list(observations=list(c(0,100)),mode="discrete", time.increment=1,time.unit="step")}
  \item For Lin's windsurfers , which is missing all observations on day 25:
  \code{list(observations=list(c(0,25),c(26,31)),mode="discrete", time.increment=1,time.unit="day")}

  \item For McFarland's classroom data:
  \code{list(observations=list(c(0,55),),mode="continuous", time.increment=NA,time.unit="minute")}

  \item A fictitious instantaneous panel example :
  \code{list(observations=list(c(0,0),c(21,21),c(56.5,56.5),c(68,68)),mode="discrete", time.increment=0,time.unit="seconds")}

The values of the \code{net.obs.period} object are not checked for consistency with activity spells of the network itself. 

}



}
\seealso{
Several of the \code{\link{networkDynamic}} converter functions set the \code{net.obs.period} attribute on their output object. See also \code{\link{activity.attribute}}
}
\references{
Link to page on statnet wiki?
}

\examples{
nd<-network.initialize(5)
activate.vertices(nd,onset=0,terminus=100)
set.network.attribute(nd,'net.obs.period',list(observations=list(c(0,100)),
                      mode="discrete", time.increment=1,time.unit="step"))
# find the overall `expected' time range for the object
range(get.network.attribute(nd,'net.obs.period')$observations)
}


