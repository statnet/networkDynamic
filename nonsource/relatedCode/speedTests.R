# this script is to test the timings of the dynamic attribute (TEA) methods
require(networkDynamic)

# adding a vertex TEA vs network size
tries = 50
max =10000
netSizes <- runif(tries,min=0,max=max)
setTimes <- numeric(tries)
setComplexTimes <- numeric(tries)
firstInsertTimes <- numeric(tries)
thirdInsertTimes<- numeric(tries)
fourthInsertTimes<- numeric(tries)
getTimes<- numeric(tries)
setEdgeTimes <-numeric(tries)
firstActivateEdgeTimes <-numeric(tries)
secondActivateEdgeTimes <-numeric(tries)
thirdActivateEdgeTimes <-numeric(tries)
getEdgeTimes <-numeric(tries)

Rprof(filename='~/SNA_health/statnet_research/networkDynamic/trunk/tea_rprof')
Rprof(NULL)

for (n in 1:tries){
  net <- network.initialize(netSizes[n])
  start <- as.numeric(Sys.time())
  activate.vertex.attribute(net,"letters","a",onset=1,terminus=2)
  firstInsertTimes[n]<-as.numeric(Sys.time())-start
  
  activate.vertex.attribute(net,"letters","c",onset=3,terminus=4)
  start <- as.numeric(Sys.time())
  activate.vertex.attribute(net,"letters","d",onset=4,terminus=5)
  thirdInsertTimes[n]<-as.numeric(Sys.time())-start
  start <- as.numeric(Sys.time())
  activate.vertex.attribute(net,"letters","b",onset=2,terminus=3)
  fourthInsertTimes[n]<-as.numeric(Sys.time())-start
  
  Rprof(filename='~/SNA_health/statnet_research/networkDynamic/trunk/tea_rprof',append=TRUE)
  start <- as.numeric(Sys.time())
  get.vertex.attribute.active(net,"letters",onset=2,terminus=3)
  getTimes[n]<-as.numeric(Sys.time())-start
  
  Rprof(NULL)
  
  start <- as.numeric(Sys.time())
  set.vertex.attribute(net,"numbers","one")
  setTimes[n]<-as.numeric(Sys.time())-start
  
  start <- as.numeric(Sys.time())
  set.vertex.attribute(net,"attribute",list(list("a","b"),c(1,2,3)))
  setComplexTimes[n]<-as.numeric(Sys.time())-start
  
  # now do stuff with edges
  add.edges(net,tail=as.list(1:(netSizes[n]-1)),head=as.list(2:netSizes[n]))
  
  start <- as.numeric(Sys.time())
  activate.edge.attribute(net,"letters","a",onset=0,terminus=1)
  firstActivateEdgeTimes[n]<-as.numeric(Sys.time())-start
  
  start <- as.numeric(Sys.time())
  activate.edge.attribute(net,"letters","b",onset=1,terminus=2)
  secondActivateEdgeTimes[n]<-as.numeric(Sys.time())-start
  
  start <- as.numeric(Sys.time())
  activate.edge.attribute(net,"letters","d",onset=3,terminus=4)
  thirdActivateEdgeTimes[n]<-as.numeric(Sys.time())-start
  
  start <- as.numeric(Sys.time())
  set.edge.attribute(net,"numbers","one")
  setEdgeTimes[n]<-as.numeric(Sys.time())-start
  
  start <- as.numeric(Sys.time())
  get.edge.value.active(net,"letters",onset=3,terminus=4)
  getEdgeTimes[n]<-as.numeric(Sys.time())-start
  
}




vertexPlots <-function(){
plot(netSizes,firstInsertTimes,main="vertex TEA operations",xlab="network size", ylab="seconds",ylim=c(0,max(getTimes)))
points(netSizes,thirdInsertTimes,col="blue")
points(netSizes,fourthInsertTimes,col="purple")
points(netSizes,getTimes,col="green")
points(netSizes,setTimes,col="red")
points(netSizes,setComplexTimes,col="pink")
}

edgePlots <-function(){
plot(netSizes,firstActivateEdgeTimes,main="Times for edge TEA operations",xlab="~num edges", ylab="seconds",ylim=c(0,max(getEdgeTimes)))
points(netSizes,setEdgeTimes,col="red")
points(netSizes,secondActivateEdgeTimes,col="blue")
points(netSizes,thirdActivateEdgeTimes,col="purple")
points(netSizes,getEdgeTimes,col="green")
}

par(mfrow=c(2,1))
vertexPlots()
edgePlots()
par(mfrow=c(1,1))

