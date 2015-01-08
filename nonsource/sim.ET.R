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
  class(x) <- class(x)[2:length(class(x))]
  obs<-x$net.obs.period
  now<-max(unlist(obs$observations))
  now
}
obs.period.incr.ET <- function(x){
  class(x) <- class(x)[2:length(class(x))]
  obs<-x$net.obs.period
  now<-max(unlist(obs$observations))
  obs$observations[[1]][2]<-now+1
  x$net.obs.period<-obs
  class(x) <- c("ET",class(x))
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
  xn%n%'net.obs.period'<-list(observations=list(c(0,1)),mode="discrete", time.increment=1,time.unit="step")
  edges <- et.to.edgelist(x$net)
  edges <- edges[order(edges[,3]),]
  tails <- edges[,1]
  heads <- edges[,2]
  add.edges(xn,tail=tails,head=heads)
  filename<-x$edgeCacheFile
  edgeChanges<-read.csv(filename,header = FALSE)
  nd <-networkDynamic(base.net=xn,edge.change=cbind(edgeChanges[,1],tails[edgeChanges[,2]],heads[edgeChanges[,2]],edgeChanges[,3]))
  nd%n%'edgeCacheFile' <- filename
  nd
}
