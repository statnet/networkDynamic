# tests for the reconcile activity functions
require(networkDynamic)
require(testthat)

# ---- reconcile.vertex.activity tests ------------
# test when called with wrong object
expect_error(reconcile.vertex.activity("net"), 'only be applied to networkDynamic objects')

# test with isolate and inactive edge
nd<-network.initialize(6)
add.edges.active(nd,tail=1:3,head=2:4,onset=1,terminus=3)
add.edges.active(nd,tail=4,head=1,onset=5,terminus=7)
add.edge(nd,tail=1,head=6)
nd2<-reconcile.vertex.activity(nd)
spls<-get.vertex.activity(nd2,as.spellList=TRUE)
expect_equal(spls$onset,c(1,1,1,1,-Inf))
expect_equal(spls$terminus,c(3,3,3,3,Inf))
expect_equal(spls$vertex.id,c(1,2,3,4,6))

# test modifiy in place
nd<-network.initialize(6)
add.edges.active(nd,tail=1:3,head=2:4,onset=1,terminus=3)
add.edges.active(nd,tail=4,head=1,onset=5,terminus=7)
add.edge(nd,tail=1,head=6)
reconcile.vertex.activity(nd)
spls<-get.vertex.activity(nd,as.spellList=TRUE)
expect_equal(spls$onset,c(1,1,1,1,-Inf),info='reconcile.vertex.activity modify in place')
expect_equal(spls$terminus,c(3,3,3,3,Inf),info='reconcile.vertex.activity modify in place')
expect_equal(spls$vertex.id,c(1,2,3,4,6),info='reconcile.vertex.activity modify in place')

# test with edge active default FALSE
nd<-network.initialize(6)
add.edges.active(nd,tail=1:3,head=2:4,onset=1,terminus=3)
add.edges.active(nd,tail=4,head=1,onset=5,terminus=7)
add.edge(nd,tail=1,head=6)
reconcile.vertex.activity(nd,edge.active.default=FALSE)
spls<-get.vertex.activity(nd,as.spellList=TRUE)
expect_equal(spls$onset,c(1,1,1,1))
expect_equal(spls$terminus,c(3,3,3,3))
expect_equal(spls$vertex.id,c(1,2,3,4))

# test with bad mode
expect_error(reconcile.vertex.activity(nd,mode='foobar'),'not supported')



