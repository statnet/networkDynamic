sim.nD.init <- function(n){
  input<-network.initialize(n)
#create net obs period
  input%n%'net.obs.period'<-list(observations=list(c(0,1)),mode="discrete", time.increment=1,time.unit="step")
  input <- as.networkDynamic(input)
  class(input) <- c("nD",class(input))
  input
}
obs.period.now.nD <- function(x){
  obs<-x%n%'net.obs.period'
  now<-max(unlist(obs$observations))
  now
}
obs.period.incr.nD <- function(x){
  obs<-x%n%'net.obs.period'
  now<-max(unlist(obs$observations))
  obs$observations[[1]][2]<-now+1
  x%n%'net.obs.period'<-obs
  x
}
net.size.nD <- function(x){
  network.size(x)
}
net.edgeIDs.nD <- function(x, v, alter){
  get.edgeIDs(x,v=v,alter=alter)
}
net.add.edges.active.nD <- function(x, tail, head, onset, terminus){
  x<-add.edges.active(x,tail=tail,head=head,onset=onset,terminus=terminus)
  x
}
net.is.vertex.active.nD <- function(x, v, at){
  is.active(x,v=v,at=at)
}
net.is.edge.active.nD <- function(x, e, at){
  is.active(x,e=e,at=at)
}

# check if edge exists and is active (non-existing edge assumed inactive)
net.is.dyad.active.nD <- function(x, tail, head, at){
  if (length(get.edgeIDs.active(x,v=tail,alter=head,at=at))>0){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# check if edge exists for dyad.  If not, add and activate
net.activate.dyad.nD <- function(x, tail, head, at){
  eid<-get.edgeIDs(x,v = tail,alter=head)
  if (length(eid)==0){
    add.edges.active(x,tail = tail,head=head,onset=at,terminus=Inf) # add and activate
  } else {
    activate.edges(x,e=eid,onset=at,terminus=Inf) # activate
  }
}

net.deactivate.dyad.nD <- function(x, tail, head, at){
  eid<-get.edgeIDs(x,v = tail,alter=head)
  if (length(eid)>0){  # make sure edge exists
    deactivate.edges(x,e=eid,onset=at,terminus=Inf)  # deactivate
  }
}




net.deactivate.edges.nD <- function(x, e, onset, terminus){
  x<-deactivate.edges(x,e=e,onset=onset,terminus=terminus)
  x
}
net.activate.edges.nD <- function(x, e, onset, terminus){
  # check if edge exists and add before activiating
  x<-activate.edges(x,e=e,onset=onset,terminus=terminus)
  x
}
net.as.networkDynamic.nD <- function(x){
  x
}
