# speed test for simulation-style edits
library(networkDynamic)
library(microbenchmark)

# Testing framework for network-type objects

# S3 class methods each network-type object must implement.
# Also need to create a sim.{network-type}.init function with signature
# sim.{network-type}.init <- function(n) {} where n is number of nodes
obs.period.now <- function(x) UseMethod("obs.period.now")
obs.period.incr <- function(x) UseMethod("obs.period.incr")
net.size <- function(x) UseMethod("net.size")
net.edgeIDs <- function(x, v, alter) UseMethod("net.edgeIDs")
net.add.edges.active <- function(x, tail, head, onset, terminus)
                  UseMethod("net.add.edges.active")
net.is.edge.active <- function(x, e, at) UseMethod("net.is.edge.active")
net.is.vertex.active <- function(x, v, at) UseMethod("net.is.vertex.active")
net.deactivate.edges <- function(x, e, onset, terminus) UseMethod("net.deactivate.edges")
net.activate.edges <- function(x, e, onset, terminus) UseMethod("net.activate.edges")
net.as.networkDynamic <- function(x) UseMethod("net.as.networkDynamic")

# Vanilla networkDynamic object
source("sim.nD.R")

# ndCache object
source("sim.ndCache.R")

# function to define each simulation step.
# this is inteded to do something analagous to a simulation involving edge and vertex dynamics
# changing attributes, etc. 
simStep<-function(x,toggleEdges=TRUE,toggleVertices=TRUE,toggleVertAttrs=TRUE){
  now<-obs.period.now(x)
#  if(toggleVertices){
    # add a vertex
#    add.vertices.active(nD,nv = 1,onset=now,terminus=Inf)
    # toggle some vertex activity
#    vtog<-sample(1:network.size(nD),size = round(network.size(nD)/50),replace = FALSE)
#    vactive<-is.active(nD,at=now,v=vtog)
#    activate.vertices(nD,v = which(!vactive),onset=now,terminus=Inf)
#    deactivate.vertices(nD,v=which(vactive),onset=now,terminus=Inf,deactivate.edges = TRUE)
#  }
  
  if(toggleEdges){
    # toggle some edges activity 
    #activeV<-which(net.is.vertex.active(nD,v=1:network.size(nD),at=now))
    #headTog<-sample(1:net.size(x),size=round(net.size(x)/25),replace=FALSE)
    #tailTog<-sample(1:net.size(x),size=round(net.size(x)/25),replace=FALSE)
    headTog<-sample(1:net.size(x),size=net.size(x),replace=FALSE)
    tailTog<-sample(1:net.size(x),size=net.size(x),replace=FALSE)
    # slow loop
    for(e in 1:length(headTog)){
      eid<-net.edgeIDs(x,v=tailTog[e],alter=headTog[e])
      if (length(eid)==0){
        x <- net.add.edges.active(x,tail =tailTog[e],head=headTog[e],onset=now,terminus=Inf)
      } else {
        if (net.is.edge.active(x,e=eid,at=now)){
          x<-net.deactivate.edges(x,e=eid,onset=now,terminus=Inf)
        } else {
          x<-net.activate.edges(x,e=eid,onset=now,terminus=Inf)
        }
      }
    }
  }
  
#  if(toggleVertAttrs){
#    # toggle some vertex attributes
#    vattrTog<-sample(1:network.size(nD),size = round(network.size(nD)/50),replace = FALSE)
#    activate.vertex.attribute(nD,"value",runif(length(vattrTog)),v=vattrTog)
#   }
  
  # increment net obs period and return updated network object
  return(obs.period.incr(x))
}

# functions to define sim runs
runSim<-function(x,steps=100,toggleEdges=TRUE,toggleVertices=TRUE,toggleVertAttrs=TRUE){
  set.seed(1)
  for(t in 1:steps){
    x<-simStep(x,toggleEdges,toggleVertices,toggleVertAttrs)
  }
  return(net.as.networkDynamic(x))
}


# Run network-type nD 
runSim.nD <- function(){
  nD<-sim.nD.init(11)
  nD <- net.add.edges.active(nD,tail=1:10,head=2:11,onset=0,terminus=Inf)
  invisible(runSim(nD))
}
microbenchmark(runSim.nD(),times = 10)

# Run network-type ndCache
runSim.ndCache <- function(){
  ndCache<-sim.ndCache.init(11)
  ndCache <- net.add.edges.active(ndCache,tail=1:10,head=2:11,onset=0,terminus=Inf)
  invisible(runSim(ndCache))
}
microbenchmark(runSim.ndCache(),times = 10)

runSimNoConvert<-function(x,steps=100,toggleEdges=TRUE,toggleVertices=TRUE,toggleVertAttrs=TRUE){
  set.seed(1)
  for(t in 1:steps){
    x<-simStep(x,toggleEdges,toggleVertices,toggleVertAttrs)
  }
  x
}

runSim.nD.NC <- function(){
  nD<-sim.nD.init(11)
  nD <- net.add.edges.active(nD,tail=1:10,head=2:11,onset=0,terminus=Inf)
  invisible(runSimNoConvert(nD))
}
microbenchmark(runSim.nD.NC(),times = 10)

# Run network-type ndCache
runSim.ndCache.NC <- function(){
  ndCache<-sim.ndCache.init(11)
  ndCache <- net.add.edges.active(ndCache,tail=1:10,head=2:11,onset=0,terminus=Inf)
  invisible(runSimNoConvert(ndCache))
}
microbenchmark(runSim.ndCache.NC(),times = 10)

# speed profiling of existing method
#Rprof(filename = 'simTest')
#output<-runNormalSim(input)
#Rprof(NULL)
#summaryRprof('simTest')
