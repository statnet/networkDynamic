# tests for utilities functions
require(networkDynamic)
require(testthat)
# ------- test for adjust.activity -----

test<-network.initialize(3)
activate.vertices(test,onset=0,terminus=3,v=1:2)
add.edges.active(test,tail=1:2,head=2:3,onset=0,terminus=3)
add.edge(test,tail=3,head=1)
activate.vertex.attribute(test,'fruit','apple',v=1:2,onset=0,terminus=3)
activate.edge.attribute(test,'veggie','carrot',e=1:2,onset=0,terminus=3)
activate.network.attribute(test,'meat','pork',onset=0,terminus=3)
test%n%'net.obs.period'<-list(observations=list(c(0,1),c(1,2),c(2,3)),mode="discrete", time.increment=1,time.unit="step")

# test offset and return value
test2<-adjust.activity(test,offset=5)

expect_equal(test2$val[[1]]$active,matrix(c(5,8),ncol=2))
expect_equal(test2$val[[3]]$active,NULL)
expect_equal(test2$mel[[1]]$atl$active,matrix(c(5,8),ncol=2))
expect_equal(test2$mel[[3]]$atl$active,NULL)
expect_equal(test2$val[[1]]$'fruit.active'[[2]],matrix(c(5,8),ncol=2))
expect_equal(test2$val[[3]]$'fruit.active'[[2]],NULL)
expect_equal(test2$mel[[1]]$atl$'veggie.active'[[2]],matrix(c(5,8),ncol=2))
expect_equal(test2$mel[[3]]$atl$'veggie.active'[[2]],NULL)
expect_equal(test2$gal$'meat.active'[[2]],matrix(c(5,8),ncol=2))
expect_equal(unlist((test2%n%'net.obs.period')$observations),c(5,6,6,7,7,8))
expect_equal((test2%n%'net.obs.period')$time.increment,1)

# test factor and modify-in-place
adjust.activity(test,factor=.5)

expect_equal(test$val[[1]]$active,matrix(c(2.5,4),ncol=2))
expect_equal(test$val[[3]]$active,NULL)
expect_equal(test$mel[[1]]$atl$active,matrix(c(2.5,4),ncol=2))
expect_equal(test$mel[[3]]$atl$active,NULL)
expect_equal(test$val[[1]]$'fruit.active'[[2]],matrix(c(2.5,4),ncol=2))
expect_equal(test$val[[3]]$'fruit.active'[[2]],NULL)
expect_equal(test$mel[[1]]$atl$'veggie.active'[[2]],matrix(c(2.5,4),ncol=2))
expect_equal(test$mel[[3]]$atl$'veggie.active'[[2]],NULL)
expect_equal(test$mel[[1]]$atl$'veggie.active'[[2]],matrix(c(2.5,4),ncol=2))
expect_equal(unlist((test%n%'net.obs.period')$observations),c(2.5,3,3,3.5,3.5,4))
expect_equal((test%n%'net.obs.period')$time.increment,0.5)

# ---- test for add.vertices.active -----
net<-network.initialize(3)
# test for adding zero vertices
add.vertices.active(net,nv=0)
expect_equal(network.size(net),3)
expect_true(is.networkDynamic(net))

add.vertices.active(net,nv=2, onset=1,terminus=2)
expect_equal(network.size(net),5)
expect_true(is.networkDynamic(net))
expect_equal(unlist(get.vertex.activity(net,as.spellList=TRUE)[4:5,1:2]),c(1,1,2,2),check.names=FALSE)
