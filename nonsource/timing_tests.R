# Though this file is in tests/, it doesn't run any tests when sourced,
# just defines some functions you can call yourself if you want to check:
#  how long is.active takes with and without C helper functions
#  whether is.active works with and without C helper functions

# timing test for is.active
# actually test timing of extract, which uses is.active
timing.test.is.active.internal <- function(anet, reps) {
  print(system.time(for(j in 1:reps){network.extract(anet, at=30)}))
}

# test network borrowed from tests/network_tests.R
timing.test.is.active.small <- function() {
  # set up the data
  anet <- network.initialize(7, directed=FALSE)
  add.edges(anet, c(1,2,3,4,4,4), c(2,4,4,6,7,5))
  activate.vertices(anet, 10, 40, v=c(1,3,4,5,7))
  deactivate.vertices(anet, v=c(2,6))
  activate.edges(anet, at=30, e=c(1,3,4,6))
  deactivate.edges(anet, e=c(2,5))
  # and do the timing tests
  timing.test.is.active.internal(anet, 2000);
}

# bigger test network, to see if it makes more difference
timing.test.is.active.large <- function() {
  # create large random network
  nv <- 1000
  ne <- 10000
  anet <- network.initialize(nv, directed=TRUE)
  add.edges(anet, sample.int(nv, size=ne, replace=TRUE), sample.int(nv, size=ne, replace=TRUE))
  # throw active spells onto vertices and edges randomly, with possible multiples
  deactivate.vertices(anet)
  deactivate.edges(anet)
  activate.vertices(anet, 
                    onset=runif(min=0,max=1000,n=nv), length=runif(min=0,max=500,n=nv),
                    v=sample.int(nv, size=nv, replace=TRUE))
  activate.edges(anet, onset=runif(min=0,max=1000,n=ne), length=runif(min=0,max=500,n=ne),
                 e=sample.int(ne, size=ne, replace=TRUE))
  # do the timing tests
  timing.test.is.active.internal(anet, 10)
}

# and test the 3 implementations against the existing network test suite
timing.test.insert.spell <- function() {
  nv <- 10000
  ne <- 30000
  ns <- 100
  nr <- 5
  anet <- network.initialize(nv, directed=TRUE)
  add.edges(anet, sample.int(nv, size=ne, replace=TRUE),
	    sample.int(nv, size=ne, replace=TRUE))
  print( system.time( for(j in 1:nr) {
    deactivate.vertices(anet)
    deactivate.edges(anet)
    activate.vertices(anet, 
                      onset=runif(min=0,max=1000,n=ns), 
		      length=runif(min=0,max=10,n=ns),
                      v=sample.int(nv, size=ns, replace=TRUE))
    activate.edges(anet, onset=runif(min=0,max=1000,n=ns), 
		   length=runif(min=0,max=10,n=ns),
                   e=sample.int(ne, size=ns, replace=TRUE))
  } ) )
}

timing.test.activate.edges <- function() {
  nv <- 10000
  ne <- 30000
  ns <- 100
  nr <- 5
  anet <- network.initialize(nv, directed=TRUE)
  add.edges(anet, sample.int(nv, size=ne, replace=TRUE),
            sample.int(nv, size=ne, replace=TRUE))
  activate.vertices(anet)
  print( system.time( for(j in 1:nr) {
    deactivate.edges(anet)
    activate.edges(anet, onset=runif(min=0,max=1000,n=ns), 
                   length=runif(min=0,max=10,n=ns),
                   e=sample.int(ne, size=ns, replace=TRUE))
  } ) )
}

library(microbenchmark)

anet<-network.initialize(1000, directed=TRUE)
ne <- 10000
add.edges(anet, sample.int(network.size(anet), size=ne, replace=TRUE),
          sample.int(network.size(anet), size=ne, replace=TRUE))

test.old.activate.edges<-function(anet,ntog=1000,ns=1000){
  for(t in 1:ns){
    anet<-r.activate.edges(anet,onset=t-1,terminus=t,e=sample(ntog,1:network.edgecount(anet)))
  }
}

test.new.activate.edges<-function(anet,ntog=1000,ns=1000){
  for(t in 1:ns){
    anet<-activate.edges(anet,onset=t-1,terminus=t,e=sample(ntog,1:network.edgecount(anet)))
  }
}

timing<-microbenchmark(test.old.activate.edges(network.copy(anet)),
                       test.new.activate.edges(network.copy(anet)),
                       times=10)

#library('network')
#timing.test.activate.edges()

