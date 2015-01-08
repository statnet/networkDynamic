sim.nD.init <- function(n){
  input<-network.initialize(n)
#create net obs period
  input%n%'net.obs.period'<-list(observations=list(c(0,1)),mode="discrete", time.increment=1,time.unit="step")
  input <- as.networkDynamic(input)
  class(input) <- c("nD",class(input))
  input
}
obs.period.now.nD <- function(x){
  class(x) <- class(x)[2:length(class(x))]
  obs<-x%n%'net.obs.period'
  now<-max(unlist(obs$observations))
  now
}
obs.period.incr.nD <- function(x){
  class(x) <- class(x)[2:length(class(x))]
  obs<-x%n%'net.obs.period'
  now<-max(unlist(obs$observations))
  obs$observations[[1]][2]<-now+1
  x%n%'net.obs.period'<-obs
  class(x) <- c("nD",class(x))
  x
}
net.size.nD <- function(x){
  class(x) <- class(x)[2:length(class(x))]
  network.size(x)
}
net.edgeIDs.nD <- function(x, v, alter){
  class(x) <- class(x)[2:length(class(x))]
  get.edgeIDs(x,v=v,alter=alter)
}
net.add.edges.active.nD <- function(x, tail, head, onset, terminus){
  class(x) <- class(x)[2:length(class(x))]
  x<-add.edges.active(x,tail=tail,head=head,onset=onset,terminus=terminus)
  class(x) <- c("nD",class(x))
  x
}
net.is.vertex.active.nD <- function(x, v, at){
  class(x) <- class(x)[2:length(class(x))]
  is.active(x,v=v,at=at)
}
net.is.edge.active.nD <- function(x, e, at){
  class(x) <- class(x)[2:length(class(x))]
  is.active(x,e=e,at=at)
}
net.deactivate.edges.nD <- function(x, e, onset, terminus){
  class(x) <- class(x)[2:length(class(x))]
  x<-deactivate.edges(x,e=e,onset=onset,terminus=terminus)
  class(x) <- c("nD",class(x))
  x
}
net.activate.edges.nD <- function(x, e, onset, terminus){
  class(x) <- class(x)[2:length(class(x))]
  x<-activate.edges(x,e=e,onset=onset,terminus=terminus)
  class(x) <- c("nD",class(x))
  x
}
net.as.networkDynamic.nD <- function(x){
  class(x) <- class(x)[2:length(class(x))]
  x
}
