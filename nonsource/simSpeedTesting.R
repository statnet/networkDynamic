# speed test for simulation-style edits
library(networkDynamic)
library(microbenchmark)



# function to define each simulation step using normal nD functions
# this is inteded to do something analagous to a simulation involving edge and vertex dynamics
# changing attributes, etc. 
simStepNormal<-function(nD,toggleEdges=TRUE,toggleVertices=TRUE,toggleVertAttrs=TRUE){
  obs<-nD%n%'net.obs.period'
  now<-max(unlist(obs$observations))
  if(toggleVertices){
    # add a vertex
    add.vertices.active(nD,nv = 1,onset=now,terminus=Inf)
    # toggle some vertex activity
    vtog<-sample(1:network.size(nD),size = round(network.size(nD)/50),replace = FALSE)
    vactive<-is.active(nD,at=now,v=vtog)
    activate.vertices(nD,v = which(!vactive),onset=now,terminus=Inf)
    deactivate.vertices(nD,v=which(vactive),onset=now,terminus=Inf,deactivate.edges = TRUE)
  }
  
  if(toggleEdges){
    # toggle some edges activity 
    activeV<-which(is.active(nD,v=1:network.size(nD),at=now))
    headTog<-sample(activeV,size=round(network.size(nD)/50),replace=FALSE)
    tailTog<-sample(activeV,size=round(network.size(nD)/50),replace=FALSE)
    # slow loop
    for(e in 1:length(headTog)){
      eid<-get.edgeIDs(nD,v=tailTog[e],alter=headTog[e])
      if (length(eid)==0){
        add.edges.active(nD,tail =tailTog[e],head=headTog[e],onset=now,terminus=Inf)
      } else {
        if (is.active(nD,e=eid,at=now)){
          deactivate.edges(nD,e=eid,onset=now,terminus=Inf)
        } else {
          activate.edges(nD,e=eid,onset=now,terminus=Inf)
        }
      }
    }
  }
  
  if(toggleVertAttrs){
    # toggle some vertex attributes
    vattrTog<-sample(1:network.size(nD),size = round(network.size(nD)/50),replace = FALSE)
    activate.vertex.attribute(nD,"value",runif(length(vattrTog)),v=vattrTog)
   }
  
  # increment net obs period
  obs$observations[[1]][2]<-now+1
  nD%n%'net.obs.period'<-obs
  return(nD)
}

# functions to define a single step of sim, using the caching method
simStepCache<-function(nD,toggleEdges=TRUE,toggleVertices=TRUE,toggleVertAttrs=TRUE){
  obs<-nD%n%'net.obs.period'
  now<-max(unlist(obs$observations))
  if(toggleVertices){
    # add a vertex
    add.vertices(nD,nv=1)
    # toggle some vertex activity
#     vtog<-sample(1:network.size(nD),size = round(network.size(nD)/50),replace = FALSE)
#     vactive<-is.active(nD,at=now,v=vtog)
#     activate.vertices(nD,v = which(!vactive),onset=now,terminus=Inf)
#     deactivate.vertices(nD,v=which(vactive),onset=now,terminus=Inf,deactivate.edges = TRUE)
  }
  
  if(toggleEdges){
    # toggle some edges activity 
    activeV<-1:network.size(nD)
    headTog<-sample(activeV,size=round(network.size(nD)/50),replace=FALSE)
    tailTog<-sample(activeV,size=round(network.size(nD)/50),replace=FALSE)
    # slow loop
    for(e in 1:length(headTog)){
      eid<-get.edgeIDs(nD,v=tailTog[e],alter=headTog[e])
      if (length(eid)==0){
        add.edges(nD,tail =tailTog[e],head=headTog[e])
        activate.edges.nDCache(nD,e=eid,at=now)
      } else {
        if (is.active(nD,e=eid,at=now)){
          deactivate.edges.nDCache(nD,e=eid,at=now)
        } else {
          activate.edges.nDCache(nD,e=eid,at=now)
        }
      }
    }
  }
  
  if(toggleVertAttrs){
  # toggle some vertex attributes
#   vattrTog<-sample(1:network.size(nD),size = round(network.size(nD)/50),replace = FALSE)
#   activate.vertex.attribute(nD,"value",runif(length(vattrTog)),v=vattrTog)
}

# increment net obs period
obs$observations[[1]][2]<-now+1
nD%n%'net.obs.period'<-obs
return(nD)
}


# functions to define sim runs
runNormalSim<-function(nD,steps=100,toggleEdges=TRUE,toggleVertices=TRUE,toggleVertAttrs=TRUE){
  for(t in 1:steps){
    nD<-simStepNormal(nD,toggleEdges,toggleVertices,toggleVertAttrs)
  }
  return(nD)
}

runCacheSim<-function(nD,steps=100,toggleEdges=TRUE,toggleVertices=TRUE,toggleVertAttrs=TRUE){
  for(t in 1:steps){
    nD<-simStepCache(nD,toggleEdges,toggleVertices,toggleVertAttrs)
  }
  return(nD)
}


# actually run the simulation test
input<-network.initialize(100)
add.edges.active(input,tail = 1:10,2:11,onset=0,terminus=Inf)
#create net obs period
input%n%'net.obs.period'<-list(observations=list(c(0,1)),mode="discrete", time.increment=1,time.unit="step")

microbenchmark(runNormalSim(input),times = 10)

# speed profiling of existing method
Rprof(filename = 'simTest')
output<-runNormalSim(input)
Rprof(NULL)
summaryRprof('simTest')


# below are sketches for functions that would cache the historical state while maintaining the "current" state of the network
# one option would that we'd matain the current state using something like a C-backed edgereee object (which is what ergm uses internally)
# but it would need to be extended to support vertex dynamics.  And we'd have to re-implement all of the network package methods to use it ...

# features:
# support edge and vertex dynamics, but only via 'at' syntax (no spells)
# support querying the state of the network at the 'current' time
# no multiplex or hyper edges
# all attributes must be simple (numeric, character, logical) no list elements

ndCache.initialize<-function(n, directed = TRUE, hyper = FALSE, loops = FALSE, multiple = FALSE, 
                   bipartite = FALSE,filename=tempfile()){
  # todo: give error if multiplex or hyper
  
  # create a network object
  net<-network.initialize(n=n,directed = directed, hyper = FALSE, loops = loops, multiple = FALSE, 
                     bipartite = bipartite)
  # ideally, we would use structures in edgetree.C instead of the network object

  net%n%'filename'<-filename
  return(net)
}

# question: should the cache for edges be based on edge id or vertex endpoints
# question: should the verticse, edges, attributes, etc all be cached to seperate files or interleaved in the same file?

activate.edges.nDCache<-function(x, onset = NULL, terminus = NULL, length = NULL, at = NULL, 
                                 e = seq_along(x$mel)){
  filename<-x%n%'filename'
  # todo: convert length and at to onset and terminus
  at<-rep(at,length.out = length(e))
  
  # write the toggle time to the cache file
  write(t(cbind(at,e,rep(1,length(e)))),file=filename,append=TRUE,ncolumns=3,sep=',')
  
  # update a vector maintaining the current state of edges?
  return(x)
}

deactivate.edges.nDCache<-function(x, onset = NULL, terminus = NULL, length = NULL, at = NULL, 
                                 e = seq_along(x$mel)){
  filename<-x%n%'filename'
  at<-rep(at,length.out = length(e))
  
  # write the toggle time to the cache file
  write(t(cbind(at,e,rep(0,length(e)))),file=filename,append=TRUE,ncolumns=3,sep=',')
  return(x)
}

# convert the cached time information in an ndCache object into a networkDynamic object
as.networkDynamic.ndCache <- function(object,...){
  # retrive the name of cache file
  filename<-object%n%'filename'
  edgeChanges<-read.csv(filename,header = FALSE)
  # look up the head and tail vertices for each edge
  heads<-sapply(object$mel,'[[','inl')
  tails<-sapply(object$mel,'[[','outl')
  nd <-networkDynamic(base.net=object,edge.change=cbind(edgeChanges[,1],tails[edgeChanges[,2]],heads[edgeChanges[,2]],edgeChanges[,3]))
  return(nd)
}



# crude test to make sure it behaves as expected
nC<-ndCache.initialize(5)
add.edges(nC,tail = 1:4,head=2:5)
activate.edges.nDCache(nC,at=0)
deactivate.edges.nDCache(nC,at=1,e=1:3)
nd<-as.networkDynamic.ndCache(nC)
get.edge.activity(nd,as.spellList = TRUE)

# comparison between the two models
input<-network.initialize(100)
inputCache<-ndCache.initialize(100)
add.edges(input,tail = 1:10,2:11)
add.edges(inputCache,tail = 1:10,2:11)
input%n%'net.obs.period'<-list(observations=list(c(0,1)),mode="discrete", time.increment=1,time.unit="step")
inputCache%n%'net.obs.period'<-list(observations=list(c(0,1)),mode="discrete", time.increment=1,time.unit="step")

microbenchmark(runNormalSim(input,steps=1000,TRUE,FALSE,FALSE),runCacheSim(inputCache,steps=1000,TRUE,FALSE,FALSE),times = 3)
  
  