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

sim.ndCache.init<-function(n){
  # create a network object
  net<-network.initialize(n=n,directed = TRUE, hyper = FALSE, loops = FALSE, multiple = FALSE, 
                     bipartite = FALSE)
  # ideally, we would use structures in edgetree.C instead of the network object

  # name of cache file to hold edge activity data
  filename <- tempfile()
  net%n%'edgeCacheFile'<-filename
  # attribute to store the current observed time range of the network
  net%n%'net.obs.period'<-list(observations=list(c(0,1)),mode="discrete", time.increment=1,time.unit="step")
  # add attribute to maintain current edge activity status
  net%n%'edgeActivity'<-logical()
  
  # alter the class of the object
  class(net)<-c('ndCache',class(net))

  return(net)
}

obs.period.now.ndCache <- function(x){
  class(x) <- class(x)[2:length(class(x))]
  obs<-x%n%'net.obs.period'
  now<-max(unlist(obs$observations))
  now
}
obs.period.incr.ndCache <- function(x){
  class(x) <- class(x)[2:length(class(x))]
  obs<-x%n%'net.obs.period'
  now<-max(unlist(obs$observations))
  obs$observations[[1]][2]<-now+1
  x%n%'net.obs.period'<-obs
  class(x) <- c("ndCache",class(x))
  x
}
net.size.ndCache <- function(x){
  class(x) <- class(x)[2:length(class(x))]
  network.size(x)
}

net.edgeIDs.ndCache <- function(x, v, alter){
  class(x) <- class(x)[2:length(class(x))]
  get.edgeIDs(x,v=v,alter=alter)
}

# question: should the cache for edges be based on edge id or vertex endpoints
# question: should the vertices, edges, attributes, etc all be cached to seperate files or interleaved in the same file?
# question: should activations automatically increment the net.obs.period?
# disallow edge/vertex deletion (since it is expensive). They may be added, or deactivated, but not removevd?

net.add.edges.active.ndCache<-function (x, tail, head, onset, terminus){
  class(x) <- class(x)[2:length(class(x))]
  x<-add.edges.network(x,tail=tail,head=head,onset=onset,terminus=terminus)  # or should this be the networkDynamic version?
  # update the edge status vector, assuming edge is active
  # We also presume that edge ids increase
  eA <- x%n%'edgeActivity'
  x%n%'edgeActivity' <- c(eA,rep(TRUE,length(x$mel)-length(eA)))
  class(x) <- c("ndCache",class(x))
  x
}

net.activate.edges.ndCache<-function(x, e, onset = NULL, terminus = NULL){
  class(x) <- class(x)[2:length(class(x))]
  filename<-x%n%'edgeCacheFile'
  # todo: convert length and at to onset and terminus
  #at<-rep(at,length.out = length(e))
  # write the toggle time to the cache file
  #write(t(cbind(at,e,rep(1,length(e)))),file=filename,append=TRUE,ncolumns=3,sep=',')
  write(t(cbind(onset,e,rep(1,length(e)))),file=filename,append=TRUE,ncolumns=3,sep=',')
  
  # update a vector maintaining the current state of edges
  edgeActivity<-x%n%'edgeActivity'
  edgeActivity[e]<-TRUE
  x%n%'edgeActivity'<-edgeActivity
  class(x) <- c("ndCache",class(x))
  x
}

net.deactivate.edges.ndCache<-function(x, e, onset, terminus){
  class(x) <- class(x)[2:length(class(x))]
  filename<-x%n%'edgeCacheFile'

  # todo: convert length and at to onset and terminus
  #at<-rep(at,length.out = length(e))
  # write the toggle time to the cache file
  #write(t(cbind(at,e,rep(0,length(e)))),file=filename,append=TRUE,ncolumns=3,sep=',')
  write(t(cbind(onset,e,rep(0,length(e)))),file=filename,append=TRUE,ncolumns=3,sep=',')

  # update a vector maintaining the current state of edges
  edgeActivity<-x%n%'edgeActivity'
  edgeActivity[e]<-FALSE
  x%n%'edgeActivity'<-edgeActivity
  class(x) <- c("ndCache",class(x))
  x
}

# only implemented for edges at the moment
net.is.edge.active.ndCache <- function (x, e, at) {
  class(x) <- class(x)[2:length(class(x))]
  # give error if any time values specefied (or if at doesn't match with net.obs.period)
  # give error for active.default=FALSE (ndCache model assumes TRUE)
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

net.is.vertex.active.ndCache <- function (x, v, at) {
  class(x) <- class(x)[2:length(class(x))]
  rep(TRUE,length(network.size(x)))
}

# convert the cached time information in an ndCache object into a networkDynamic object
net.as.networkDynamic.ndCache <- function(x){
  class(x) <- class(x)[2:length(class(x))]
  # retrive the name of cache file
  filename<-x%n%'edgeCacheFile'
  edgeChanges<-read.csv(filename,header = FALSE)
  # look up the head and tail vertices for each edge
  heads<-sapply(x$mel,'[[','inl')
  tails<-sapply(x$mel,'[[','outl')
  nd <-networkDynamic(base.net=x,edge.change=cbind(edgeChanges[,1],tails[edgeChanges[,2]],heads[edgeChanges[,2]],edgeChanges[,3]))
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
    activeE <- is.active.ndCache(x = x, onset = onset, terminus = terminus, 
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
#nC<-ndCache.initialize(5)
#nC<-add.edges.nDCache(nC,tail = 1:4,head=2:5)
#nC<-activate.edges.nDCache(nC,at=0)
#nC<-deactivate.edges.nDCache(nC,at=1,e=1:3)
#nC<-activate.edges.nDCache(nC,e=1,at=3)
#is.active.nDCache(nC,e=1:4)
#nd<-as.networkDynamic.ndCache(nC)
#get.edge.activity(nd,as.spellList = TRUE)
#network.collapse.ndCache(nC)

# comparison between the two models
#input<-network.initialize(100)
#inputCache<-ndCache.initialize(100)
#add.edges(input,tail = 1:10,2:11)
#add.edges(inputCache,tail = 1:10,2:11)
#input%n%'net.obs.period'<-list(observations=list(c(0,1)),mode="discrete", time.increment=1,time.unit="step")
#inputCache%n%'net.obs.period'<-list(observations=list(c(0,1)),mode="discrete", time.increment=1,time.unit="step")

#microbenchmark(runNormalSim(input,steps=1000,TRUE,FALSE,FALSE),runCacheSim(inputCache,steps=1000,TRUE,FALSE,FALSE),times = 3)
