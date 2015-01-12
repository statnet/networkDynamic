
#WARNING:  THE *.dyad.* methods now use a different model for how edgetree should work than the and and remove edges version. 

# sim.{network-type}.init <- function(n) {} where n is number of nodes
print.ET <- function(x, ...){
  class(x) <- 'list'
  print(x)
  invisible(x)
}
sim.ET.init <- function(n){

  et <- list()

  et$net <- et.initialize(0,0,0,n,1)

  # name of cache file to hold edge activity data
  et$edgeCacheFile <- tempfile()
  # attribute to store the current observed time range of the network
  et$net.obs.period<-list(observations=list(c(0,1)),mode="discrete", time.increment=1,time.unit="step")
  # add attribute to maintain current edge activity status
  et$edgeActivity<-logical()
  
  # alter the class of the object
  class(et)<-c('ET',class(et))

  et
}
obs.period.now.ET <- function(x){
  obs<-x$net.obs.period
  now<-max(unlist(obs$observations))
  now
}
obs.period.incr.ET <- function(x){
  obs<-x$net.obs.period
  now<-max(unlist(obs$observations))
  obs$observations[[1]][2]<-now+1
  x$net.obs.period<-obs
  x
}
net.size.ET <- function(x) et.info(x$net)$nnodes
net.edgeIDs.ET <- function(x, v, alter) {
  ans <- 
    unlist(lapply(1:length(v), function(i){
      et.get.eid(x$net,v[i],alter[i])
    }))
  ans[ans!=0]
}
net.add.edges.active.ET <- function(x, tail, head, onset, terminus){
  et.add.edges(tail,head,x$net)

  eA <- x$edgeActivity

  # Eids defined by old length of edgeActivity and nedges
  eids <- seq.int(length(eA)+1,et.info(x$net)$nedges)

  # Update edge activity vector
  x$edgeActivity <- c(eA,rep(TRUE,length(eids)))

  # Now record activations
  net.activate.edges.ET(x,eids,onset=onset,terminus=terminus)

  x
}
net.is.edge.active.ET <- function(x, e, at){
  x$edgeActivity[e]
}
net.is.vertex.active.ET <- function(x, v, at){
  rep(TRUE,length(v))
}


net.is.dyad.active.ET <- function(x, tail, head, at){
  # this will return 0 if edge does not exist in edgetree
  eid<-et.get.eid(x$net,tail,head)
  if (eid>0){
    return(TRUE)
  } else {
    return(FALSE) # can't find edge
  }
}
net.activate.dyad.ET <- function(x, tail, head, at){
  eid<-et.get.eid(x$net,tail,head)
  if (eid == 0){
    # update the edge tree object
    # don't have to save result back to x$net, because it is just a pointer so value doesn't change
    et.add.edges(tail,head,x$net)
    # write the update out to cache file
    filename<-x$edgeCacheFile
    write(t(cbind(at,tail,head,1)),file=filename,append=TRUE,ncolumns=4,sep=',')
    
    
  } else {
    warning("dyad already active with eid",eid)
  }
  x
}

net.deactivate.dyad.ET<- function(x, tail, head, at){
  eid<-et.get.eid(x$net,tail,head)
  if (eid == 0){
    warning("dyad already inactive")
    
  } else {
    filename<-x$edgeCacheFile
    # write the update out to cache file
    write(t(cbind(at,tail,head,0)),file=filename,append=TRUE,ncolumns=4,sep=',')
    # update the edge tree object
    et.delete.edges(tail,head,x$net)
  }
  x
}

net.activate.edges.ET <- function(x, e, onset, terminus){
  filename<-x$edgeCacheFile
  # todo: convert length and at to onset and terminus
  #at<-rep(at,length.out = length(e))
  # write the toggle time to the cache file
  #write(t(cbind(at,e,rep(1,length(e)))),file=filename,append=TRUE,ncolumns=3,sep=',')
  write(t(cbind(onset,e,rep(1,length(e)))),file=filename,append=TRUE,ncolumns=3,sep=',')
  
  # update a vector maintaining the current state of edges
  edgeActivity<-x$edgeActivity
  edgeActivity[e]<-TRUE
  x$edgeActivity<-edgeActivity
  x
}
net.deactivate.edges.ET <- function(x, e, onset, terminus){
  filename<-x$edgeCacheFile

  # todo: convert length and at to onset and terminus
  #at<-rep(at,length.out = length(e))
  # write the toggle time to the cache file
  #write(t(cbind(at,e,rep(0,length(e)))),file=filename,append=TRUE,ncolumns=3,sep=',')
  write(t(cbind(onset,e,rep(0,length(e)))),file=filename,append=TRUE,ncolumns=3,sep=',')

  # update a vector maintaining the current state of edges
  edgeActivity<-x$edgeActivity
  edgeActivity[e]<-FALSE
  x$edgeActivity<-edgeActivity
  x
}
net.as.networkDynamic.ET <- function(x) {
  xn <- network.initialize(et.info(x$net)$nnodes)
  filename<-x$edgeCacheFile
  edgeChanges<-read.csv(filename,header = FALSE)
  nd <-networkDynamic(base.net=xn,edge.changes=edgeChanges)
  nd%n%'edgeCacheFile' <- filename
  nd
}
