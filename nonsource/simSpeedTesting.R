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
# idea is generally that networkDynamic effectively becomes an API of S3 dispatched methods, which are either implemented by the existing networkDynamic class, or the nDCache class
# one option would that we'd matain the current state using something like a C-backed edgereee object (which is what ergm uses internally) but it would need to be extended to support vertex dynamics.  And we'd have to re-implement all of the network package methods to use it ...
# currently using a single logical vector to represent edge activity state, but it is attached to the network as a network-level attribute, so any modifications will presumeably induce copies.  Perhaps all of this could be maintained in C, with only a single pointer cached on the network?

# features:
# support edge and vertex dynamics, but only via 'at' syntax (no spells)
# support querying the state of the network at the 'current' time
# no multiplex or hyper edges
# all attributes must be simple (numeric, character, logical) no list elements

# NOTE: if we use net.obs.period to cache the curent 'at' state, need to adjust extraction times 
# back one step unit?
# NOTE: assign-in-parent functionality not currently implemented!

ndCache.initialize<-function(n, directed = TRUE, hyper = FALSE, loops = FALSE, multiple = FALSE, 
                   bipartite = FALSE,filename=tempfile(),at=0){
  # todo: give error if multiplex or hyper
  
  # create a network object
  net<-network.initialize(n=n,directed = directed, hyper = FALSE, loops = loops, multiple = FALSE, 
                     bipartite = bipartite)
  # ideally, we would use structures in edgetree.C instead of the network object

  # name of cache file to hold edge activity data
  net%n%'edgeCacheFile'<-filename
  # attribute to store the current observed time range of the network
  if (is.null(net%n%'net.obs.period')){
    net%n%'net.obs.period'<-list(observations=list(c(at,at)),mode="discrete", time.increment=1,time.unit="step")
  }
  # add attribute to maintain current edge activity status
  net%n%'edgeActivity'<-logical()
  
  # alter the class of the object
  class(net)<-c('ndCache',class(net))

  return(net)
}

# question: should the cache for edges be based on edge id or vertex endpoints
# question: should the vertices, edges, attributes, etc all be cached to seperate files or interleaved in the same file?
# question: should activations automatically increment the net.obs.period?
# disallow edge/vertex deletion (since it is expensive). They may be added, or deactivated, but not removevd?

add.edges.nDCache<-function (x, tail, head, names.eval = NULL, vals.eval = NULL, ...){
  x<-add.edges.network(x,tail,head,names.eval,vals.eval,...)  # or should this be the networkDynamic version?
  # update the edge status vector, assuming edge is active
  x%n%'edgeActivity'<-c(x%n%'edgeActivity',TRUE)
  invisible(x)
}

activate.edges.nDCache<-function(x, onset = NULL, terminus = NULL, length = NULL, at = NULL, 
                                 e = seq_along(x$mel)){
  filename<-x%n%'edgeCacheFile'
  # todo: convert length and at to onset and terminus
  at<-rep(at,length.out = length(e))
  
  # write the toggle time to the cache file
  write(t(cbind(at,e,rep(1,length(e)))),file=filename,append=TRUE,ncolumns=3,sep=',')
  
  # update a vector maintaining the current state of edges
  edgeActivity<-x%n%'edgeActivity'
  edgeActivity[e]<-TRUE
  x%n%'edgeActivity'<-edgeActivity
  return(x)
}

deactivate.edges.nDCache<-function(x, onset = NULL, terminus = NULL, length = NULL, at = NULL, 
                                 e = seq_along(x$mel)){
  filename<-x%n%'edgeCacheFile'
  at<-rep(at,length.out = length(e))
  
  # write the toggle time to the cache file
  write(t(cbind(at,e,rep(0,length(e)))),file=filename,append=TRUE,ncolumns=3,sep=',')
  # update a vector maintaining the current state of edges
  edgeActivity<-x%n%'edgeActivity'
  edgeActivity[e]<-FALSE
  x%n%'edgeActivity'<-edgeActivity
  return(x)
}

# only implemented for edges at the moment
is.active.nDCache <- function (x, onset = NULL, terminus = NULL, length = NULL, at = NULL, 
          e = NULL, v = NULL, rule = c("any", "all", "earliest", "latest"), 
          active.default = TRUE){
  # give error if any time values specefied (or if at doesn't match with net.obs.period)
  # give error for active.default=FALSE (nDCache model assumes TRUE)
  # ignore 'rule'
  
  if (length(e)) {
    origelen <- length(e)
    e <- e[!sapply(x$mel[e], is.null)]
    if (length(e) < origelen) {
      warning("Some edge IDs in the e argument correspond to deleted edges and will be ignored. Indices of values returned will not correspond to elements of e.")
    }
  }
  edgeActivity<-x%n%'edgeActivity'
  return(edgeActivity[e])
}

# convert the cached time information in an ndCache object into a networkDynamic object
as.networkDynamic.ndCache <- function(object,...){
  # retrive the name of cache file
  filename<-object%n%'edgeCacheFile'
  edgeChanges<-read.csv(filename,header = FALSE)
  # look up the head and tail vertices for each edge
  heads<-sapply(object$mel,'[[','inl')
  tails<-sapply(object$mel,'[[','outl')
  nd <-networkDynamic(base.net=object,edge.change=cbind(edgeChanges[,1],tails[edgeChanges[,2]],heads[edgeChanges[,2]],edgeChanges[,3]))
  return(nd)
}

# sketch of a function to collapse the network down to its state at the last timepoint and return as a static network object
# (some confusion here if, for compatibility with networkDynamic, this would be 'extract' or 'collapse')
network.collapse.ndCache<-function (x, onset = NULL, terminus = NULL, length = NULL, at = NULL, 
          rule = c("any", "all"), active.default = TRUE, retain.all.vertices = FALSE, 
          trim.spells = FALSE) {
  #TODO: at must be checked (and should default to) the last timepoint of the network
  # assesed via net obs period
  
  activeV <- 1:network.size(x) # not yet implemented, so assume always active
  activeE = logical(0)
  if (length(x$mel)) {
    activeE <- is.active.nDCache(x = x, onset = onset, terminus = terminus, 
                         length = length, at = at, e = valid.eids(x), v = NULL, 
                         rule = rule, active.default = active.default)
    nullE <- sapply(x$mel, is.null)
    inV = sapply(x$mel[!nullE], "[[", "inl")
    outV = sapply(x$mel[!nullE], "[[", "outl")
    activeTH = sapply(1:length(inV), function(x) {
      activeV[inV[x]] && activeV[outV[x]]
    })
  }
  if (retain.all.vertices) {
    newVid <- seq.int(x %n% "n")
  }
  else {
    newVid <- cumsum(activeV)
  }
  if (!is.null(at)) {
    onset <- terminus <- at
  }
  else if (!is.null(onset)) {
    if (!is.null(length)) 
      terminus <- onset + length
  }
  else {
    if (is.null(terminus)) {
      onset <- -Inf
      terminus <- Inf
    }
    else {
      onset <- terminus - length
    }
  }
  n <- ifelse(retain.all.vertices, x %n% "n", sum(activeV))
  net <- network.initialize(n)
  gal <- as.list(x$gal)
  gal[["n"]] <- n
  gal[["mnext"]] <- 1
  # remove timing and file connection info
  if (!is.null(gal[['edgeCacheFile']])){
    gal[['edgeCacheFile']]<-NULL
  }
  if (!is.null(gal[['edgeActivity']])){
    gal[['edgeActivity']]<-NULL
  }
  
  if (is.bipartite(x) && gal[["bipartite"]] > 0) {
    gal[["bipartite"]] <- newVid[gal[["bipartite"]]]
  }
  if (!is.null(gal[["net.obs.period"]])) {
    gal[["net.obs.period"]]<- NULL
  }
  set.network.attribute(net, names(gal), gal)
  if (n > 0) {
    if (retain.all.vertices) {
      net$val <- as.list(x$val)
    }
    else {
      net$val <- as.list(x$val[activeV])
    }
  }
  if (length(activeE)) {
    activeETH = activeE & activeTH
    if (any(activeETH)) {
      tail <- as.list(lapply(x$mel[!nullE][activeETH], 
                             function(z) {
                               newVid[z$outl]
                             }))
      head <- as.list(lapply(x$mel[!nullE][activeETH], 
                             function(z) {
                               newVid[z$inl]
                             }))
      atl <- as.list(lapply(x$mel[!nullE][activeETH], "[[", 
                            "atl"))
      nam <- as.list(lapply(x$mel[!nullE][activeETH], function(z) {
        names(z$atl)
      }))
      add.edges(net, tail = tail, head = head, names.eval = nam, 
                vals.eval = atl)
    }
  }
  return(net)
}



# ----- crude test to make sure it behaves as expected ------
nC<-ndCache.initialize(5)
nC<-add.edges.nDCache(nC,tail = 1:4,head=2:5)
nC<-activate.edges.nDCache(nC,at=0)
nC<-deactivate.edges.nDCache(nC,at=1,e=1:3)
nC<-activate.edges.nDCache(nC,e=1,at=3)
is.active.nDCache(nC,e=1:4)
nd<-as.networkDynamic.ndCache(nC)
get.edge.activity(nd,as.spellList = TRUE)
network.collapse.ndCache(nC)

# comparison between the two models
input<-network.initialize(100)
inputCache<-ndCache.initialize(100)
add.edges(input,tail = 1:10,2:11)
add.edges(inputCache,tail = 1:10,2:11)
input%n%'net.obs.period'<-list(observations=list(c(0,1)),mode="discrete", time.increment=1,time.unit="step")
inputCache%n%'net.obs.period'<-list(observations=list(c(0,1)),mode="discrete", time.increment=1,time.unit="step")

microbenchmark(runNormalSim(input,steps=1000,TRUE,FALSE,FALSE),runCacheSim(inputCache,steps=1000,TRUE,FALSE,FALSE),times = 3)
  
  