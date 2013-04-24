########################################################################
# This file contains the testing suite for the "network" methods, i.e.:
#
#   network.extract              network.edgecount.active
#   network.dynamic.check        network.naedgecount.active
#   network.dyadcount.active     network.size.active   
#########################################################################

require(networkDynamic)


#------------------------ NETWORK.EXTRACT ----------------------------
# Notes:
#  - I've tested combinations of retain.all.vertices with bipartite
#    and non-bipartite networks, directed and undirected networks,
#    networks w/ and w/o loops or multi-edges.  I have not tested
#    what would happen if users added onto the basic network structure,
#    or what happens if edges/nodes are missing.
#--------------------------------------------------------------------

cat("testing network.extract ... ")

# a network with no active anything and no edges
anet0 <- network.initialize(7, directed=FALSE)
bnet0=network.extract(anet0, at=10)
b0 = (bnet0%n%"n" == 7)
# a network with no active anything
anet1 <- network.initialize(7, directed=FALSE)
add.edges(anet1, c(1,2,3,4,4,4),
                 c(2,4,4,6,7,5))
anet.copy <- anet1
deactivate.edges(anet1)
deactivate.vertices(anet1)
bnet1=network.extract(anet1, at=10)
bnet1.t=anet1%t%10
b1 = identical(bnet1, list())
b2 = identical(bnet1, bnet1.t)
# a network with no active edges, but active vertices, retain.all=F
anet2 <- anet.copy
activate.vertices(anet2, 10, 20, v=c(1,3,4))
deactivate.vertices(anet2, v=c(2,4,5,7))
deactivate.edges(anet2)
bnet2=network.extract(anet2, at=10)
bnet2.t=anet2%t%10
b2 = (bnet2%n%"n" == 3)
b3 = (bnet2%n%"mnext" == 1)
b4 = identical(bnet2, bnet2.t)
# a network with no active edges, but active vertices, retain.all=T
anet3 <- anet2
bnet3=network.extract(anet3, at=10, retain.all.vertices=TRUE)
b5 = (bnet3%n%"n" == 7)
b6 = (bnet3%n%"mnext" == 1)
# a network with no active vertices, but active edges
anet4 <- anet.copy
activate.vertices(anet4, 10, Inf)
activate.edges(anet4, -Inf, 20)
bnet4=network.extract(anet4, -10, 0)
b7 = identical(bnet4, list())
# networks with active edges and vertices
#  a non-bipartite simple network, retain.all=F
anet5 <- anet.copy
activate.vertices(anet5, 10, 40, v=c(1,3,4,5,7))
deactivate.vertices(anet5, v=c(2,6))
activate.edges(anet5, at=30, e=c(1,3,4,6))
deactivate.edges(anet5, e=c(2,5))
bnet5=network.extract(anet5, at=30)
bnet5.t=anet5%t%30
el5 = as.matrix(bnet5, matrix.type="edgelist")
b8 = (bnet5%n%"n" == 5)
b9 = (bnet5%n%"mnext" == 3)
b10 = all(el5==matrix(c(2,3,3,4),2,2))
b11 = identical(bnet5, bnet5.t)
#  a non-bipartite simple network, retain.all=T
anet6 <- anet5
bnet6=network.extract(anet6, at=30, retain.all=TRUE)
el6 = as.matrix(bnet6, matrix.type="edgelist")
b12 = (bnet6%n%"n" == 7)
b13 = (bnet6%n%"mnext" == 3)
b14 = all(el6==matrix(c(3,4,4,5),2,2))
# bipartite simple network, retain.all = F
anet7 <- network.initialize(10, bipartite=6)
add.edges(anet7, c(1,2,3,4,4,6), c(7,8,7,8,9,10))
activate.vertices(anet7, -Inf, Inf, v=c(1,3,6,7,9,10))
activate.edges(anet7, 10, Inf, e=c(1,3,5))
bnet7=network.extract(anet7, 0, 20, active.default=FALSE)
el7 = as.matrix(bnet7, matrix.type="edgelist")
b15 = (bnet7%n%"n" == 6)
b16 = (bnet7%n%"mnext" == 3)
b17 = (bnet7%n%"bipartite"== 3)
b18 = all(el7==matrix(c(1,2,4,4),2,2))
# bipartite simple network, retain.all = T
anet8 <- anet7
bnet8=network.extract(anet8, 0, 20, active.default=FALSE, retain.all.vertices=T)
el8 = as.matrix(bnet8, matrix.type="edgelist")
b19 = (bnet8%n%"n" == 10)
b20 = (bnet8%n%"mnext" == 3)
b21 = (bnet8%n%"bipartite"== 6)
b22 = all(el8==matrix(c(1,3,7,7),2,2))
# a non-bipartite network, with loops, retain.all=F
anet9 <- anet.copy
add.edges(anet9, c(2,5,6), c(2,5,6))
activate.vertices(anet9, v=c(1,2,4,6))
deactivate.vertices(anet9, v=c(3,5,7))
activate.edges(anet9, e=c(1,2,6,7,8))
deactivate.edges(anet9, e=c(3,4,5,9))
bnet9=network.extract(anet9, at=-Inf)
bnet9.t=anet9%t%-Inf
el9 = as.matrix(bnet9, matrix.type="edgelist")
b23 = (bnet9%n%"n" == 4)
b24 = (bnet9%n%"mnext" == 4)
b25 = all(el9==matrix(c(1,2,2,2,3,2),3,2))
b26 = identical(bnet9, bnet9.t)
# a non-bipartite network, with loops, retain.all=F
anet10 <- anet9
bnet10=network.extract(anet10, at=Inf, retain.all.vertices=T)
el10 = as.matrix(bnet10, matrix.type="edgelist")
b27 = (bnet10%n%"n" == 7)
b28 = (bnet10%n%"mnext" == 4)
b29 = all(el10==matrix(c(1,2,2,2,4,2),3,2))
# a non-bipartite network, with multi-edges, retain.all=F
anet11 <- network.initialize(8)
add.edges(anet11, c(1,1,1,1,1,4,4,5,5,6,6,8),
                  c(3,3,3,6,2,2,5,4,8,7,7,5))
activate.vertices(anet11, 10,20, v=c(1,3,4,5,6,7))
deactivate.vertices(anet11, v=c(2,8))
activate.edges(anet11, 10, 15, e=c(1:3,7,9,11))
deactivate.edges(anet11, e=c(4,5,6,8,10,12))
bnet11=network.extract(anet11, 11,14)
el11 = as.matrix(bnet11, matrix.type="edgelist")
b30 = (bnet11%n%"n" == 6)
b31 = (bnet11%n%"mnext" == 6)
b32 = all(el11==matrix(c(1,1,1,3,5,2,2,2,4,6),5,2))
# a non-bipartite network, with multi-edges, retain.all=T
anet12 <- anet11
bnet12=network.extract(anet12, 14,19, retain.all.vertices=T)
el12 = as.matrix(bnet12, matrix.type="edgelist")
b33 = (bnet12%n%"n" == 8)
b34 = (bnet12%n%"mnext" == 6)
b35 = all(el12==matrix(c(1,1,1,4,6,3,3,3,5,7),5,2))
# pavel's test with nullified edges
data(flo)
anet13 <- network(flo)
anet13[15,] = 0   # remove all out edges from node 15
el13 <- as.matrix(anet13, matrix.type="edgelist")
deactivate.edges(anet13)
activate.edges(anet13, e=seq(2,40,2))
bnet13 <- anet13 %t% 5
el13.b1 <- el13[c(2,4,6,8,9,seq(12,24,2),seq(28,36,2)),]
el13.b2 <- as.matrix(bnet13, matrix.type="edgelist")
b36 = (bnet13%n%"n" == 16)
b37 = all(el13.b1==el13.b2)


b.tests = paste("b", seq(0,37), sep="")
b.results= sapply(b.tests, function(x){eval(parse(text=x))})
if(any(!b.results)){
  bad.tests = paste("b", which(!b.results), sep="", collapse=" ")
  stop(paste("network.extract is incorrectly extracting networks in tests",
             bad.tests))
}
cat("ok\n")


#-------------------- NETWORK.DYNAMIC.CHECK ------------------------
# Notes:
#  - 
#--------------------------------------------------------------------

cat("testing network.dynamic.check ... ")

# a network with no activity specified at all
cnet0 <- network.initialize(5)
add.edges(cnet0, c(1,3,5), c(4,4,2))
check0 = network.dynamic.check(cnet0, verbose=F)
c0 = (sum(!check0$vertex.checks)==0 &&
      sum(!check0$edge.checks)==0)

# a network with complete inactivity specified
cnet1 <- cnet0
deactivate.edges(cnet1)
deactivate.vertices(cnet1)
check1 = network.dynamic.check(cnet1, verbose=F)
c1 = (sum(!check1$vertex.checks)==0 &&
      sum(!check1$edge.checks)==0)

# a network that is A-okay
cnet2 <- cnet1
activate.edges(cnet2, 10,20, e=1:2)
activate.vertices(cnet2, 0,30)
check2 = network.dynamic.check(cnet2, verbose=F)
c2 = (sum(!check2$vertex.checks)==0 &&
      sum(!check2$edge.checks)==0 &&
      sum(!check2$dyad.checks)==0)

# a network with illegal node activity matrices
cnet3 <- cnet2
cnet3$val[[2]]$active <- matrix(c(10,0,20,5),2,2)
cnet3$val[[3]]$active <- matrix(c(10,15,20,25),2,2)
cnet3$val[[4]]$active <- matrix(c(10,40,20,30),2,2)
check3 = network.dynamic.check(cnet3, verbose=F)
c3 = (all(check3$vertex.checks==c(T,F,F,F,T)) &&
      sum(!check3$edge.checks)==0)

# a network with illegal edge activity matrices
cnet4 <- cnet2
cnet4$mel[[1]]$atl$active <- matrix(c(10,0,20,5),2,2)
cnet4$mel[[2]]$atl$active <- matrix(c(10,15,20,25),2,2)
cnet4$mel[[3]]$atl$active <- matrix(c(10,40,20,30),2,2)
check4 = network.dynamic.check(cnet4, verbose=F)
c4 = (sum(!check4$vertex.checks)==0 &&
      sum(check4$edge.checks)==0)

# a network with active edges that have inactive end points
cnet5 <- cnet1
activate.edges(cnet5,10,20)
activate.vertices(cnet5,0,3, v=c(1,3,4))
activate.vertices(cnet5,0,30, v=c(2,5))
check5 = network.dynamic.check(cnet5, verbose=F)
c5 = all(check5$dyad.checks==c(F,F,T))


# a network with multiple problems
cnet6 <- cnet5
cnet6$val[[2]]$active <- matrix(c(10,0,20,5),2,2)
cnet6$mel[[3]]$atl$active <- matrix(c(10,15,20,25),2,2)
check6 = network.dynamic.check(cnet6, verbose=F)
c6 = (check6$vertex.checks[2]==F &&
      check6$edge.checks[3]==F)


# a network with a point-activated edge
cnet7 <- network.initialize(2)
cnet7[1,2]<-1
activate.edges(cnet7,at=1)
check7<-network.dynamic.check(cnet7)
c7 <- (check7$vertex.checks==c(T,T) &&
       check7$edge.checks==T &&
       check7$dyad.checks==T)

c.tests = paste("c", seq(1,7), sep="")
c.results= sapply(c.tests, function(x){eval(parse(text=x))})
if(any(!c.results)){
  bad.tests = paste("c", which(!c.results), sep="", collapse=" ")
  stop(paste("network.dynamic.check is returning incorrect results in tests",
             bad.tests))
}
cat("ok\n")




#---------------- NETWORK.EDGECOUNT.ACTIVE --------------------------
# Notes:
#  - this function also relies primarily on 'is.active', so the testing
#    done here is minimal
#----------------------------------------------------------------------

cat("testing network.edgecount.active ... ")
data(flo)
net0 <- network.initialize(3)
net1 <- network(flo)
net2 <- net1
for (i in 1:5)
  net1$mel[[i]]$atl$na = TRUE
for (i in 1:8)
  net2$mel[[i]]$atl$na = TRUE
deactivate.edges(net1)
deactivate.edges(net2)
activate.edges(net1, 10,20, e=3:7)
activate.edges(net2, 10,20, e=3:7)
b1=(network.edgecount.active(net1, 5,15)==2)            # a positive count, na.omit=T
b2=(network.edgecount.active(net1, 5,15, na.omit=F)==5) # a positive count, na.omit=F
b3=(network.edgecount.active(net1, 40, Inf)==0)         # a 0 count b/c of non-activity
b4=(network.edgecount.active(net2, 5,15)==0)            # a 0 count b/c of missing edges
b5=(network.edgecount.active(net0, 5,15)==0)            # a 0 count b/c of no edges

b.tests = paste("b", seq(1,5), sep="")
b.results= sapply(b.tests, function(x){eval(parse(text=x))})
if(any(!b.results)){
  bad.tests = paste("b", which(!b.results), sep="", collapse=" ")
  warning(paste("network.edgecount.active is incorrectly counting edges in tests",
             bad.tests))
}
cat("ok\n")


#---------------- NETWORK.NAEDGECOUNT.ACTIVE --------------------------
# Notes:
#  - this function also relies primarily on 'is.active', so the testing
#    done here is minimal
#----------------------------------------------------------------------
cat("testing network.naedgecount.active ... ")
data(flo)
# a network with missing edges
net3 <- network(flo)
net4 <- net3
for (i in 1:5)
  net3$mel[[i]]$atl$na = TRUE
deactivate.edges(net3)
activate.edges(net3, 10,20, e=3:7)
# a network without missing edges
deactivate.edges(net4)
activate.edges(net4, 10,20, e=3:7)
# a network without missing edges, but with nullified edges
net5 <- net4
net5$mel[[1]] <- NULL
net5$mel[[2]] <- NULL
# a network with missing edges and with nullified edges
net6 <- net3
net6$mel[[1]] <- NULL
net6$mel[[2]] <- NULL

b1=(network.naedgecount.active(net3, 5,15)==3)     # a positive count
b2=(network.naedgecount.active(net6, 5,15)==2)     # a positive count
b3=(network.naedgecount.active(net3, 40, Inf)==0)  # a 0 count b/c of non-activity
b4=(network.naedgecount.active(net4, 40, Inf)==0)  # a 0 count b/c of non-activity
b5=(network.naedgecount.active(net5, 40, Inf)==0)  # a 0 count b/c of non-activity
b6=(network.naedgecount.active(net6, 40, Inf)==0)  # a 0 count b/c of non-activity
b7=(network.naedgecount.active(net4, 5,15)==0)     # a 0 count b/c of no missing edges
b8=(network.naedgecount.active(net5, 5,15)==0)     # a 0 count b/c of no missing edges
b9=(network.naedgecount.active(net0, 5,15)==0)     # a 0 count b/c of no edges

b.tests = paste("b", seq(1,9), sep="")
b.results= sapply(b.tests, function(x){eval(parse(text=x))})
if(any(!b.results)){
  bad.tests = paste("b", which(!b.results), sep="", collapse=" ")
  warning(paste("network.naedgecount.active is incorrectly counting edges in tests",
             bad.tests))
}
cat("ok\n")

#-------------------- NETWORK.SIZE.ACTIVE --------------------------
# Notes:
#  - this function contains a single line and hinges on 'is.active'
#    returning the right thing, so in the interest of time, I am 
#    running 2 simple tests.
#--------------------------------------------------------------------
cat("testing network.size.active ... ")
data(flo)
net6 <- network(flo)
deactivate.vertices(net6)
activate.vertices(net6,-Inf,20, v=1:10)
b1=(network.size.active(net6, 5,15)==10)    # a positive count
b2=(network.size.active(net6, 40, Inf)==0)  # a 0 count b/c of non-activity

b.tests = paste("b", seq(1,2), sep="")
b.results= sapply(b.tests, function(x){eval(parse(text=x))})
if(any(!b.results)){
  bad.tests = paste("b", which(!b.results), sep="", collapse=" ")
  warning(paste("network.size.active is incorrectly counting nodes in tests",
             bad.tests))
}

cat("ok\n")


#---------------- NETWORK.DYADCOUNT.ACTIVE --------------------------
# Notes:
#  - this function also relies primarily on 'is.active', so again,
#    we'll test using a white box approach.
#----------------------------------------------------------------------

cat("testing network.dyadcount.active ... ")

# a directed network, no missing edges
net7 <- network.initialize(6)
activate.vertices(net7, 10, 20, v=1:4)
activate.vertices(net7, 0, 10, v=4:5)
activate.vertices(net7, 30, 40, v=6)
b1 = (network.dyadcount.active(net7, 50, Inf)==0)  # a 0 count, b/c of nonactivity
b2 = (network.dyadcount.active(net7, at=35)==0)    # a 0 count, b/c of only 1 active node
b3 = (network.dyadcount.active(net7,  0, 10)==2)   # a 2 count
b4 = (network.dyadcount.active(net7, 10, 15)==12)  # a >2 count
# an directed bipartitie network, no missing edges
net8 <- network.initialize(10, bipartite=7)
activate.vertices(net8, 10, 40, v=1:6)
activate.vertices(net8, 0, 10, v=7:8)
activate.vertices(net8, 10, 30, v=9)
activate.vertices(net8, 50, 60, v=10)
b5 = (network.dyadcount.active(net8, 80, 90)==0)   # a 0 count, b/c of nonactivity
b6 = (network.dyadcount.active(net8, 50, 55)==0)   # a 0 count, b/c of only 1 active node
b7 = (network.dyadcount.active(net8, 0, 10)==2)    # a 2 count
b8 = (network.dyadcount.active(net8, 10, 15)==12)  # a >2 count
# an undirected bipartitie network, no missing edges
net9 <- network.initialize(10, bipartite=7, directed=FALSE)
activate.vertices(net9, 10, 40, v=1:6)
activate.vertices(net9, 0, 10, v=7:8)
activate.vertices(net9, 10, 30, v=9)
activate.vertices(net9, 10, 40, v=10)
b9 = (network.dyadcount.active(net9, 40, 50)==0)    # a 0 count
b10 = (network.dyadcount.active(net9, 0, 10)==1)    # a 1 count
b11 = (network.dyadcount.active(net9, 10, 15)==12)  # a >1 count
# an undirected non-bipartitie network, no missing edges
net10 <- network(flo, directed=FALSE)
activate.vertices(net10, 20, Inf, v=1:8)
activate.vertices(net10, 10, 10, v=9:14)
b12 = (network.dyadcount.active(net10, at=0, active.default=F)==0)   # a 0 count
b13 = (network.dyadcount.active(net10, at=11)==1)                    # a 1 count
b14 = (network.dyadcount.active(net10, at=30)==45)                   # a >1 count
# a directed non-bipartite network with missing edges
add.edges(net7, c(4,5,1), c(5,4,4))
for (i in 1:3)
  net7$mel[[i]]$atl$na=TRUE
b15 = (network.dyadcount.active(net7, at=15)==11)            # a >1 count, na.omit=T
b16 = (network.dyadcount.active(net7, at=15, na.omit=F)==12) # a >1 count, na.omit=F
b17 = (network.dyadcount.active(net7, at=5)==0)              # a 0 count b/c of missing edges
# a directed bipartite network with missing edges
add.edges(net8, c(3,8,7), c(9,7,8))
for (i in 1:3)
  net8$mel[[i]]$atl$na=TRUE
b18 = (network.dyadcount.active(net8, 25,30)==11)            # a >1 count, na.omit=T
b19 = (network.dyadcount.active(net8, 25,30, na.omit=F)==12) # a >1 count, na.omit=F
b20 = (network.dyadcount.active(net8, 2,3)==0)               # a 0 count b/c of missing edges
# an undirected bipartitie network with missing edges
add.edges(net9, c(4,5,7), c(10,10,8))
for (i in 1:3)
  net9$mel[[i]]$atl$na=TRUE
b21 = (network.dyadcount.active(net9, 31,32)==4)            # a >1 count, na.omit=T
b22 = (network.dyadcount.active(net9, 31,32, na.omit=F)==6) # a >1 count, na.omit=F
b23 = (network.dyadcount.active(net9, 2,3)==0)              # a 0 count b/c of missing edges
# an undirected non-bipartitie network with missing edges
net11 <- network.initialize(16, directed=FALSE)
activate.vertices(net11, 20, Inf, v=1:8)
activate.vertices(net11, 10, 10, v=9:14)
add.edges(net11, c(15, 9), c(16,11))
activate.edges(net11)
for (i in 1:2)
  net11$mel[[i]]$atl$na=TRUE
b24 = (network.dyadcount.active(net11, at=10, active.default=F)==14)  # a >1 count, na.omit=T
b25 = (network.dyadcount.active(net11, at=10, na.omit=F)==28)         # a >1 count, na.omit=F
b26 = (network.dyadcount.active(net11, -5,0)==0)                      # a 0 count b/c of missing edges

b.tests = paste("b", seq(1,26), sep="")
b.results= sapply(b.tests, function(x){eval(parse(text=x))})
if(any(!b.results)){
  bad.tests = paste("b", which(!b.results), sep="", collapse=" ")
  stop(paste("network.dyadcount.active is incorrectly counting dyads in tests",
             bad.tests))
}

cat("ok\n")

