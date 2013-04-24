\name{ndConverter}
\alias{ndConverter}
\alias{ndConverter.default}
\alias{ndConverter.network.list}
\alias{ndConverter.data.frame}
\alias{ndConverter.list}
\alias{ndConverter.network}
\alias{duration.matrix}

\title{
ndConverter
}
\description{
S3 class function for converting between various dynamic network formats and networkDynamic objects and back again.
}
\usage{

%\method{ndConverter}{default}(x,...)

\method{ndConverter}{network.list}(x,...)

\method{ndConverter}{data.frame}(x,...)

\method{ndConverter}{list}(x,...)

\method{ndConverter}{network}(x,format="edgelist",...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{various}
  \item{format}{"edgelist", more to come....}
  \item{\dots}{ignored right now}
}
\details{
This documentation express the basic utility functions for converting between networkDynamic and other forms of dynamic networks. These are a collection of S3 objects and use the S3 dispatch form to decide which ndConverter function to use automatically. Currently conversion for \code{network.list} is for use with the \code{network.list} object in \code{\link[ergm]{ergm}} package. Note these functions are still in development and are quite fragile. 

\itemize{
\item \code{ndConverter} Generic converter function, handles dispatching based on class of the object.
\item \code{ndConverternetwork.list} Converts network.list objects into networkDynamic objects 
\item \code{ndConverter.data.frame} Converts data.frame objects into networkDynamic objects. In practice this assumes a 6 column matrix (Ego Alter Start End Noncensored duration).
\item \code{ndConverter.list} This assumes either a list of network objects (e.g., the beach data) or two part list composed of a data.frame object one with vertex information and one with edge information (e.g., Skye's .son format) [There is a proposal to alter the spec to edge.list and vertex.list where the user may provide just a list of vertices and time information, just edges list and time information or both]
\item \code{ndConverter.network} This converst networkDynamic object into data.frame objects of the following form: (Ego Alter Start End Noncensored duration).
}


}
\value{
\code{networkDynamic}, \code{\link{network}}, \code{\link{data.frame}}, \code{\link{list}}.
}
%\references{
%
%}
\author{
Zack W Almquist <almquist@uci.edu>
}
\note{
NOTE: MORE DETAILS TO COME, THIS IS NOT FINAL MANUAL PAGE FOR THESE FUNCTIONS.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
See Also as \code{\link{duration.matrix}}, \code{\link{activity.attribute}}
}
\examples{

###
### List of networks example
###
data(windsurferPanels)
dynBeach<-ndConverter(beach)
dynBeach

\dontrun{
##################
## modified from dynsim_test.R in ergm tests
##################
library(ergm)
logit<-function(p)log(p/(1-p))
coef.form.f<-function(coef.diss,density) -log(((1+exp(coef.diss))/(density/(1-density)))-1)
S<-100000
n<-200
m<-100
target.stats<-edges<-100
duration<-1000
coef.diss<-logit(1-1/duration)

### Undirected

dyads<-n*(n-1)/2
density<-edges/dyads
coef.form<-coef.form.f(coef.diss,density)

g0<-network.initialize(n,dir=FALSE)

# Get a reasonably close starting network.
set.seed(119)
g1<-san(g0~edges,target.stats=target.stats,verbose=TRUE)

dynsim<-simulate(g1,formation=~edges,dissolution=~edges,coef.form=coef.form,coef.diss=coef.diss,nsim=S,verbose=TRUE)

elexamp<-duration.matrix(dynsim)

###########
### network.list example
###########
class(dynsim)
nsdn<-ndConverter(dynsim)
nsdn
nsdn$mel[[6]]

###########
### edgelist + toggle example
###########
class(elexamp)
eldn<-ndConverter(elexamp)
eldn
eldn$mel[[6]]

elel<-ndConverter(eldn)
head(elel)
}





}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
