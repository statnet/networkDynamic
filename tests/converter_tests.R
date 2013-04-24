#test ndConverter functionality
#AFTER IT INITALY PASSES TESTS, ALL 'warning' should be switched to 'stop'

  
require(networkDynamic)
#try converting the newcomb panel data
data(newcomb)
newDyn <- ndConverter(newcomb$networks)
#does it pass a consistency check?
check <- network.dynamic.check(newDyn) 
#is the matrix equal to the input matrix

if (!all(as.sociomatrix(newcomb$networks[[1]]) == as.sociomatrix(network.extract(newDyn,onset=0,terminus=1)))){
  warning("FAIL: ndConverter: 1st input matrix does not match crosssection from time 0 to 1 for newcomb example")
}


#try converting a list that includes different size networks.
data(windsurferPanels)
dynBeach<-ndConverter(beach)

#check if the neighborhood is the same for both
ng1 <-get.neighborhood(beach[[1]],v=1)
vDyn <- as.numeric(beach[[1]]%v%'vertex.names')[1]
#data level node indicies are stored as the vertex names
ng2 <-get.neighborhood(network.extract(dynBeach,onset=0,terminus=1,retain.all.vertices=T),v=vDyn)
if(!all(ng1 == ng2)){
  stop("FAIL: ndConverter: neigborhoods of 1st vertex do not match for first time point for variable sized network list example (windsurfers)")
}


#try the version that has edge weights
newRankDyn <-ndConverter(newcomb.rank$networks)


#a really crude edgelist example
edgetimes <- as.data.frame(matrix( c(1,2,0,1, 2,3,1,2,  1,2,3,4 ),ncol=4,byrow=T))
edgetimetest<-ndConverter(edgetimes)

#does the internal representation match?
if( !all(as.vector(edgetimetest$mel[[1]]$atl$active) == c(0,3,1,4))){
  stop("FAIL: ndConverter: timed edgelist converter gave unexpected internal representation of multiple spells")
}

#lets try passing in a noncensored and duration, see if we get the same things back
edgetimes <- as.data.frame(matrix( c(1,2,0,1,0,1,  1,2,3,4,1,1, 2,3,1,2,1,1, 3,4,3,3,1,0 ),ncol=6,byrow=T))
edgetimetest<-ndConverter(edgetimes)
edgetimesout <- ndConverter(edgetimetest,format="edgelist")
if (!all(edgetimes == edgetimesout)){
  warning("FAIL: ndConverter: didn't get the same result back out when converting edge time list with noncensored to network and back to edge time list")
}

# does it preserve network attributes of passed in network
d1 <- network.initialize(2,directed=F,bipartite=T,multiple=T,loops=T)
d2 <- network.initialize(2,directed=F,bipartite=T,multiple=T,loops=T)
dlist <- list(d1,d2)
ddyn <- ndConverter(dlist)
if (is.directed(ddyn) != FALSE){
  stop("FAIL: ndConverter : 'directed' argument if initial network in list not respected in dynamic version")
}
if (is.bipartite(ddyn) != TRUE){
  stop("FAIL: ndConverter : 'bipartite' argument if initial network in list not respected in dynamic version")
}
if (is.multiplex(ddyn) != TRUE){
  stop("FAIL: ndConverter : 'multiple' argument if initial network in list not respected in dynamic version")
}
if (has.loops(ddyn) != TRUE){
  stop("FAIL: ndConverter : 'loops' argument if initial network in list not respected in dynamic version")
}


# does it warn if network attributes of passed in networks do not match
d1 <- network.initialize(2,directed=F)
d2 <- network.initialize(2,directed=T)
dlist <- list(d1,d2)
ddyn <- ndConverter(dlist)
#not sure how to flag this test

#can we pass in additional arguments to network construction? (directedness etc)
edgetimetest<-ndConverter(edgetimes,directed=FALSE)
if (is.directed(edgetimetest) != FALSE){
  warning("FAIL: ndConverter : 'directed' argument not passed to network construction")
}


#what about an array with the wrong dimensions? (no timing information)

edgetimes <- as.data.frame(matrix( c(1,2,  1,2, 2,3, 3,4 ),ncol=2,byrow=T))
try(edgetimetest <- ndConverter(edgetimes))




