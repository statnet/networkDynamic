#test networkDynamic conversion functionality
#AFTER IT INITALY PASSES TESTS, ALL 'warning' should be switched to 'stop'

#NOTE MANY THESE TESTS ARE FOR FEATURES THAT ARE NOT FULLY IMPLEMNTED FOR THE V0.3 RELEASE  

require(networkDynamic)

#disabled until 0.4

if (FALSE){

# ===============
# testing as.networkDynaic.network.list 
# 
#try converting the newcomb panel data (working 9/3)
data(newcomb)
newDyn <- as.networkDynamic(newcomb)
#does it pass a consistency check?
check <- network.dynamic.check(newDyn) 
#is the matrix equal to the input matrix
for (k in 1:length(newcomb)) {
  if (!all(as.sociomatrix(newcomb[[k]]) == as.sociomatrix(network.extract(newDyn,onset=k-1,terminus=k)))){
    stop("FAIL: ndConverter: 1st input matrix does not match crosssection from time 0 to 1 for newcomb example")
  }
}
as.data.frame(newDyn)


# try converting a list that includes different size networks. (working 9/10)
# note that beach[[25]] is NA (missing)
data(windsurferPanels)
dynBeach<-as.networkDynamic(beach)

#data level node indicies are stored as the vertex names

#check if the neighborhood is the same for both.
for (i in 1:length(beach)) {
  if (!identical(beach[[i]], NA)) {
    for (j in network.vertex.names(beach[[i]])) {
      ng1 <-get.neighborhood(beach[[i]], v=networkDynamic:::get.vertex.id(beach[[i]], j))
      ng2 <- get.neighborhood.active(dynBeach, onset=i-1, terminus=i, v=networkDynamic:::get.vertex.id(dynBeach, j))
      # the following line is much much slower
      #ng2 <-get.neighborhood(network.extract(dynBeach,onset=i-1,terminus=i,retain.all.vertices=T),v=get.vertex.id(dynBeach, j))
      # need to check the vertex names, not the ids which are changed when converting to a networkDynamic object
      names1 <- sort(network.vertex.names(beach[[i]])[ng1])
      names2 <- sort(network.vertex.names(dynBeach)[ng2])
      # print these if necessary
      # print(paste('============ ', i, '=========='))
      # print(names1)
      # print(names2)
      if (!identical(names1, names2)) {
        print(paste("FAIL: ndConverter: neigborhoods do not match for variable sized network list example (windsurfers)",
                   " at time", i, 'vertex', j))
        print(names1)
        print(names2)
      }
    }
    
  }
}


#try the version that has edge weights
newRankDyn <-as.networkDynamic(newcomb.rank)



# check correct spells printed for edges
test <- network.initialize(3)
test[1,2]<-1
test[2,3]<-1
activate.edges(test,onset=1,terminus=2)
ref <- matrix(c(1,2,1,2,0,0,1,1, 1,2,2,3,0,0,1,2),ncol=8,byrow=TRUE)
if (!all(as.data.frame(test) == ref)){
  stop("unexpected output for edge spells from as.networkDynamic.data.frame") 
}

test <- network.initialize(3)
test[1,2]<-1
test[2,3]<-1
activate.edges(test,at=3)
ref <- matrix(c(3,3,1,2,0,0,0,1, 3,3,2,3,0,0,0,2),ncol=8,byrow=TRUE)
if (!all(as.data.frame(test) == ref)){
  stop("unexpected output for edge spells of zero length from as.networkDynamic.data.frame") 
}

# test for missing activity attribute (only set on one edge)
test <- network.initialize(3)
test[1,2]<-1
test[2,3]<-1
activate.edges(test,at=3,e=1)
as.data.frame(test)
tryCatch(
  as.data.frame(test), error = function(e){ warning(paste("error in as.networkDynamic.data.frame  for edge with missing activity attribute",e))} )


#check for duration for funny length spells
test <- network.initialize(3)
test[1,2]<-1
test[2,3]<-1
activate.edges(test,at=1,e=1)
activate.edges(test,onset=2.7,terminus=5, e=2)
ref <- matrix(c(1.0,1,1,2,0,0,0,1, 2.7,5,2,3,0,0,2.3,2),ncol=8,byrow=TRUE)
if (!all(as.data.frame(test) == ref)){
  stop("unexpected output for edge spells from as.networkDynamic.data.frame") 
}

# check multiple spells per edge
test <- network.initialize(3)
test[1,2]<-1
activate.edges(test,onset=1,terminus=2)
activate.edges(test,onset=3,terminus=4)
ref <- matrix(c(1,2,1,2,0,0,1,1, 3,4,1,2,0,0,1,1),ncol=8,byrow=TRUE)
if (!all(as.data.frame(test) == ref)){
  stop("unexpected output for multiple spells per edge for as.networkDynamic.data.frame") 
}

# check censoring arguments when passed in
test <- network.initialize(3)
test[1,2]<-1
activate.edges(test,onset=-Inf,terminus=10)
ref <- matrix(c(5,10,1,2,1,0,5,1),ncol=8,byrow=TRUE)
if (!all(as.data.frame(test,start=5) == ref)){
  stop("unexpected output for 'start' left censoring argument from as.networkDynamic.data.frame") 
}

test <- network.initialize(3)
test[1,2]<-1
activate.edges(test,onset=0,terminus=Inf)
ref <- matrix(c(0,5,1,2,0,1,5,1),ncol=8,byrow=TRUE)
if (!all(as.data.frame(test,end=5) == ref)){
  stop("unexpected output for 'end' left censoring argument from as.networkDynamic.data.frame") 
}

test <- network.initialize(3)
test[1,2]<-1
activate.edges(test,onset=-Inf,terminus=Inf)
ref <- matrix(c(-Inf,Inf,1,2,0,0,Inf, 1),ncol=8,byrow=TRUE)
if (!all(as.data.frame(test) == ref)){
  warning("unexpected output for as.networkDynamic.data.frame: Inf and -Inf times treated as censored") 
}
# Li: why is this a warning? seems to be working as intended?

# check censoring arguments when set on input object using attr
test <- network.initialize(3)
test[1,2]<-1
activate.edges(test,onset=-Inf,terminus=Inf)
attr(test,"start")<-5
as.data.frame(test)
#skye: this seems to work, but I want to remove the feature



# =============== TESTING as.networkDynamic.data.frame() ===========================

# working 9/3/2012. Just compare the data frame output with edgetimes
# a really crude edgelist example
edgetimes <- as.data.frame(matrix( c(1,2,1,2, 2,3,1,3,  1,2,3,4 ),ncol=4,byrow=TRUE))
edgetimetest<-as.networkDynamic(edgetimes)
# do the edges and spells match when spit back out?
if (!all(as.data.frame(edgetimetest)[,1:4]==edgetimes)){
  stop("output spell matrix does not match input for as.networkDynamic.data.frame()")
}

#does the internal representation match?
if( !all(as.vector(edgetimetest$mel[[1]]$atl$active) == c(1,2))){
  stop("as.networkDynamic.data.frame gave unexpected internal representation spells")
}


# combining multiple spells (should combine the spells for edge between v1 and v2)
edgetimes <- as.data.frame(matrix( c(1,2,1,2, 2,3,1,2,  1,2,3,4 ),ncol=4,byrow=TRUE))
edgetimetest<-as.networkDynamic(edgetimes)
if (!all(as.data.frame(edgetimetest)[,1:4]==matrix(c(1,3,1,2, 1,2,3,4),ncol=4,byrow=TRUE))){
  stop("output spell matrix did not merge input spells as expected for as.networkDynamic.data.frame()")
}


# with censoring
# Skye:  why does input of Inf mean that it is right censored?
edgetimes <- as.data.frame(matrix( c(1,Inf,1,2, 2,3,2,3),ncol=4,byrow=TRUE))
edgetimetest<-as.networkDynamic(edgetimes)
as.data.frame(edgetimetest)
network.vertex.names(edgetimetest)

# with missing node (should fill in the missing node)
edgetimes <- as.data.frame(matrix( c(1,2,1,2, 2,4,1,2,  1,2,1,4 ),ncol=4,byrow=TRUE))
edgetimetest<-as.networkDynamic(edgetimes)
if (network.size(edgetimetest)!=4){
  stop("as.networkDynamic.data.frame() did not create network with implied size of 4")
}

# create correct number of nodes with n argument
edgetimes <- as.data.frame(matrix( c(1,2,1,2, 2,4,1,2,  1,2,1,4 ),ncol=4,byrow=TRUE))
edgetimetest<-as.networkDynamic(edgetimes,n=10)
if (network.size(edgetimetest)!=10){
  stop("as.networkDynamic.data.frame() did not create network with size required by n argument")
}


# create with vertex dynamics specified
# nodeInfo: should be a dataframe of the format
#   vertex.id   onset   terminus   NodeId(aka vertex name)
# nodetimes <-as.data.frame(matrix( c(1,1,2,1, 2,2,3,2,  3,3,4,3 ),ncol=4,byrow=TRUE))
# edgetimes <- as.data.frame(matrix( c(1,2,1,2, 2,4,1,2,  1,2,1,4 ),ncol=4,byrow=TRUE))
# as.networkDynamic(edgetimes,nodeInfo=nodetimes)

# ====================== TESTING as.networkDynamic.list

# does it create a dynamic network
d1 <- network.initialize(3)
d1[1,2]<-1
d2 <- network.initialize(3)
d2[1,2]<-1
d2[2,3]<-1
d3 <- network.initialize(3)
d3[3,1]<-1
ddyn <- as.networkDynamic(list(d1,d2,d3))

if(!is.networkDynamic(ddyn)){
  stop("as.networkDynamic.list didn't create a dynamic network from ")
}

# check that correct spells with unit lengths were created
ref <- matrix(c(0,2,1,2,0,0,2, 1,2,2,3,0,0,1, 2,3,3,1,0,0,1),ncol=7,byrow=TRUE)
if (!all(as.data.frame(ddyn)==ref)){
  stop("correct unit length spells were not created for list input networks in as.networkDynamic.list")
}

# does it preserve network attributes of passed in network
d1 <- network.initialize(2,directed=F,bipartite=T,multiple=T,loops=T)
d2 <- network.initialize(2,directed=F,bipartite=T,multiple=T,loops=T)
dlist <- list(d1,d2)
ddyn <- as.networkDynamic(dlist)

if (is.directed(ddyn) != FALSE){
  stop("FAIL: as.networkDynamic : 'directed' argument if initial network in list not respected in dynamic version")
}
if (is.bipartite(ddyn) != TRUE){
  stop("FAIL: as.networkDynamic : 'bipartite' argument if initial network in list not respected in dynamic version")
}
if (is.multiplex(ddyn) != TRUE){
  stop("FAIL: as.networkDynamic : 'multiple' argument if initial network in list not respected in dynamic version")
}
if (has.loops(ddyn) != TRUE){
  stop("FAIL: as.networkDynamic : 'loops' argument if initial network in list not respected in dynamic version")
}


# does it warn if network attributes of passed in networks do not match
# working 9/10
d1 <- network.initialize(2,directed=F)
d2 <- network.initialize(2,directed=T)
dlist <- list(d1,d2)
as.networkDynamic(dlist)

#not sure how to flag this test

#can we pass in additional arguments to network construction? (directedness etc)
# working 9/10
edgetimes <- as.data.frame(matrix( c(1,2,1,2, 2,3,1,3,  1,2,3,4 ),ncol=4,byrow=TRUE))
edgetimetest<-as.networkDynamic(edgetimes,directed=FALSE)
if (is.directed(edgetimetest) != FALSE){
  stop("FAIL: as.networkDynamic : 'directed' argument not passed to network construction")
}


#what about an array with the wrong dimensions? (no timing information)
# returns a networkDynamic with no timing info (just the base network)
edgetimes <- as.data.frame(matrix( c(1,2,  1,2, 2,3, 3,4 ),ncol=2,byrow=TRUE))
try(edgetimetest <- as.networkDynamic(edgetimes), T)


}  # disabled tests

# =============== TESTING as.data.frame.networkDynamic ====

#lets try passing in a noncensored and duration, see if we get the same things back
edgetimes <- as.data.frame(matrix( c(1,2,1,2,0,0,  1,2,3,4,0,0, 3,5,1,2,0,0, 3,4,2,4,0,0 ),ncol=6,byrow=TRUE))
colnames(edgetimes)<-c("onset","terminus","tail","head","onset.censored","terminus.censored")
testnet <- network.initialize(4)
add.edges.active(testnet,onset=1,terminus=2,tail=1,head=2)
add.edges.active(testnet,onset=1,terminus=2,tail=3,head=4)
add.edges.active(testnet,onset=3,terminus=5,tail=1,head=2)
add.edges.active(testnet,onset=3,terminus=4,tail=2,head=4)
# these should match
if (!all(as.data.frame(testnet)[,1:6] == edgetimes)){
  stop("FAIL: output data.frame from as.data.frame.networkDynamic did not match input")
}

#check column names
if(!all(names(as.data.frame(testnet))==c("onset","terminus","tail","head","onset.censored","terminus.censored","duration","edge.id"))){
  stop("Unexpected column names returned by as.data.frame.networkDynamic")
  
}

# censoring should set the appropriate start or end to Inf
# skye: Is this the behavior we want for as.data.frame?
edgetimes <- as.data.frame(matrix( c(0,2,1,2,1,0,2,  1,2,3,4,0,0,1, 3,5,1,2,0,0,2, 3,6,2,4,0,1,3 ),ncol=7,byrow=TRUE))
colnames(edgetimes)<-c("onset","terminus","tail","head","onset.censored","terminus.censored","duration")
testnet <- network.initialize(4)
add.edges.active(testnet,onset=-Inf,terminus=2,tail=1,head=2)
add.edges.active(testnet,onset=1,terminus=2,tail=3,head=4)
add.edges.active(testnet,onset=3,terminus=5,tail=1,head=2)
add.edges.active(testnet,onset=3,terminus=Inf,tail=2,head=4)

# these should match
if(!all(as.data.frame(testnet,start=0,end=6)[-8]==edgetimes)){
  stop("as.data.frame.networkDynamic gave unexpected censored spell matrix output")
}


# properly handle edges with no spell activity
test <- network.initialize(3)
test[1,2]<-1
test[2,3]<-1
activate.edges(test,at=3,e=1)
tryCatch(as.data.frame(test), error = function(e) stop("as.networkDynamic.data.frame() did not handle edges with no spell activity"))

# properly handle nD object with no edge spell activity at all
net <-network.initialize(3)
net[1,2]<-1;
net[2,3]<-1;
activate.vertices(net, onset=1, terminus=Inf)
temp = as.data.frame(net)
if (nrow(temp) != 0) {
  stop("as.data.frame.networkDynamic() did not handle an object without any edge activity")
}

# test networkDynamic (function used by TERGM)

test <- network.initialize(3)
test[1,3]<-1
tog <- matrix(c(1,1,2, 1,2,3, 2,1,2, 4,1,3, 4,1,2), ncol=3, byrow=TRUE)
net<-networkDynamic(base.net=test,edge.toggles=tog)
spells <-matrix(unlist(get.edge.activity(net)),ncol=2,byrow=T)
# first spell should start at -Inf because edge was in original net
if (!all(spells[1,]==c(-Inf,4))){
  stop("networkDynamic() did not record initial toggle correctly")
}
# 2nd spell toggles twice
if (!all(spells[2,]==c(1,4))){
  stop("networkDynamic() did not record double toggle correctly")
}
if (!all(spells[3,]==c(2,Inf)) | !all(spells[4,]==c(1,Inf))){
  stop("networkDynamic() did not record toggles correctly")
}



# ==================== TESTING duration.matrix
# this function is used internally by as.networkDynamic.network() and should not be called by user
net <-network.initialize(3)
net[1,2]<-1;
net[2,3]<-1;
net[1,3]<-1;
# toggle list: time, tail, head
tog<-matrix(c(1,1,2, 1,2,3, 2,1,2, 4,1,3, 4,1,2), ncol=3, byrow=TRUE)
# we expect this matrix
ref <- matrix(c(0,1,1,2,1,0,1, 0,1,2,3,1,0,1, 0,4,1,3,1,0,4, 2,4,1,2,0,0,2),ncol=7,byrow=TRUE)
if (!all(networkDynamic:::duration.matrix(net, changes=tog, start=0, end=5)==ref)){
  stop("duration.matrix returned an unexpected spell list for its input toggles")
}
# testing start and end
ref1 <- matrix(c(1,1,1,2,1,0,0, 1,1,2,3,1,0,0, 1,4,1,3,1,0,3, 2,4,1,2,0,0,2),ncol=7,byrow=TRUE)
if (!all(networkDynamic:::duration.matrix(net, changes=tog, start=1, end=5)==ref1)){
  stop("duration.matrix returned an unexpected spell list for its input toggles")
}
# testing start and end
ref2 <- matrix(c(0,1,1,2,1,0,1, 0,1,2,3,1,0,1, 0,4,1,3,1,0,4, 2,4,1,2,0,0,2),ncol=7,byrow=TRUE)
if (!all(networkDynamic:::duration.matrix(net, changes=tog, start=0, end=8)==ref2)){
  stop("duration.matrix returned an unexpected spell list for its input toggles")
}