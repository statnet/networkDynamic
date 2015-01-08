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

# Test for networkDynamic equality: only test edges and activity
nD.is.equal <- function(x,y){
  if (network.size(x) != network.size(y)) return(FALSE)
  if (length(x$mel) != length(y$mel)) return(FALSE)
  all(unlist(lapply(1:length(x$mel), function(i){
      x$mel[[i]]$inl == y$mel[[i]]$inl && x$mel[[i]]$outl == y$mel[[i]]$outl && 
        all(x$mel[[i]]$atl$active == y$mel[[i]]$atl$active)
  })))
}

# Vanilla networkDynamic object
source("sim.nD.R")

# ndCache object: network + file backed store
source("sim.ndCache.R")

# ET object: edgetree + file backed store
source("sim.ET.R")

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

# Sim steps plus conversion to networkDynamic
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

# Run network-type ndCache
runSim.ndCache <- function(){
  ndCache<-sim.ndCache.init(11)
  ndCache <- net.add.edges.active(ndCache,tail=1:10,head=2:11,onset=0,terminus=Inf)
  invisible(runSim(ndCache))
}

# Run network-type edgetree
runSim.ET <- function(){
  et<-sim.ET.init(11)
  et <- net.add.edges.active(et,tail=1:10,head=2:11,onset=0,terminus=Inf)
  invisible(runSim(et))
}
mb <- microbenchmark(x<-runSim.ET(),y<-runSim.nD(), z<-runSim.ndCache(), times = 10)
nD.is.equal(x,y)
nD.is.equal(y,z)
nD.is.equal(x,z)
print(mb)

# Sim steps only... no conversion to networkDynamic
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

# Run network-type ndCache
runSim.ndCache.NC <- function(){
  ndCache<-sim.ndCache.init(11)
  ndCache <- net.add.edges.active(ndCache,tail=1:10,head=2:11,onset=0,terminus=Inf)
  invisible(runSimNoConvert(ndCache))
}

# Run network-type edgetree
runSim.ET.NC <- function(){
  et<-sim.ET.init(11)
  et <- net.add.edges.active(et,tail=1:10,head=2:11,onset=0,terminus=Inf)
  invisible(runSimNoConvert(et))
}
mb <- microbenchmark(x<-runSim.ET.NC(),y<-runSim.nD.NC(), z<-runSim.ndCache.NC(), times = 10)
print(mb)


# speed profiling of existing method
#Rprof(filename = 'simTest')
#output<-runNormalSim(input)
#Rprof(NULL)
#summaryRprof('simTest')
