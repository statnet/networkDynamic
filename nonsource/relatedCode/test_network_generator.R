# generate a series of networks that include lots of edge cases to be used for testing

nd_test_nets<-list()

# non-dynamic network
net0<-network.initialize(5)
add.edges(net0,tail=1:2,head=3:4)
nd_test_nets[['non-dynamic network']]<-net0

# network with 0 vertices
net1<-as.networkDynamic(network.initialize(0))
nd_test_nets[['0 vertices']]<-net1


# network with 0 edges
net2<-as.networkDynamic(network.initialize(3))
nd_test_nets[['0 edges']]<-net2

# network with 1 vertex
net3<-as.networkDynamic(network.initialize(1))
nd_test_nets[['single vertex']]<-net3

# network with 1 edge 
net3a<-as.networkDynamic(network.initialize(3))
add.edge(net3a,tail=3,head=2)
nd_test_nets[['one edge']]<-net3a

# network with 1 edge activated
net3b<-as.networkDynamic(network.initialize(3))
add.edges.active(net3b,tail=3,head=2,onset=-Inf,terminus=Inf)
nd_test_nets[['one edge explicitly activated']]<-net3b

# network with vertices explicitly deactivated
net4<-network.initialize(3)
net4<-deactivate.vertices(net4)
nd_test_nets[['vertices explicitly deactivated']]<-net4

# network with edges that are explicitly deactivated
net5<-network.initialize(3)
add.edges(net5,tail=1:2,head=2:3)
deactivate.edges(net5)
nd_test_nets[['edges that are explicitly deactivated']]<-net5

# network in which not all edges have activation spells
net6<-network.initialize(5)
add.edges(net6,tail=1:4,head=2:5)
activate.edges(net6,onset=0,terminus=5,e=2:3)
nd_test_nets[['not all edges have activation spells']]<-net6

# network in which not all vertices have activation spells
net7<-network.initialize(5)
activate.vertices(net7,onset=0,terminus=5,v=2:4)
nd_test_nets[['not all vertices have activation spells']]<-net7

# network with some edges deleted (nulls in edgelist)
net8<-network.initialize(7)
add.edges.active(net8,tail=1:6,head=2:7,onset=0,terminus=5)
delete.edges(net8,eid=3:5)
nd_test_nets[['some edges deleted (nulls in edgelist)']]<-net8

# network with some vertices deleted (vertex names will be out of line)
net9<-as.networkDynamic(network.initialize(7))
delete.vertices(net9,vid=2:5)
nd_test_nets[['some vertices deleted (vertex names will be out of line)']]<-net9


# network with multiple edge activation spells
net10<-network.initialize(5)
add.edges(net10,tail=1:5,head=2:5)
activate.edges(net10,onset=0,terminus=1)
activate.edges(net10,onset=2,terminus=3)
nd_test_nets[['multiple edge activation spells']]<-net10

# network with multiple vertex activation spells
net11<-network.initialize(5)
activate.vertices(net11,onset=0,terminus=1)
activate.vertices(net11,onset=2,terminus=3)
nd_test_nets[['multiple vertex activation spells']]<-net11

# network in which edge and vertex activations do not sync up
net12<-network.initialize(5)
activate.vertices(net12,onset=0,terminus=1)
add.edges(net12,tail=1:5,head=2:5)
activate.edges(net12,onset=5,terminus=6)
nd_test_nets[['edge and vertex activations do not sync up']]<-net12


# network in which edge and vertex activations have negative values
net13<-network.initialize(5)
activate.vertices(net13,onset=-10,terminus=11)
add.edges(net13,tail=1:5,head=2:5)
activate.edges(net13,onset=-10,terminus=-1)
nd_test_nets[['edge and vertex activations have negative values']]<-net13

# hyper network with flag set
net14<-as.networkDynamic(network.initialize(5,hyper=TRUE))
add.edge(net14,tail=1:3,head=4:5)
nd_test_nets[['hyper network with flag set']]<-net14

# hyper network with flag not set
net14<-as.networkDynamic(network.initialize(5))
add.edge(net14,tail=1:3,head=4:5)
nd_test_nets[['hyper network with flag NOT set']]<-net14

# multiplex network with flag set
net15<-as.networkDynamic(network.initialize(5,multiple=TRUE))
add.edge(net15,tail=1:3,head=4:5)
add.edge(net15,tail=1:3,head=4:5)
nd_test_nets[['multiplex network']]<-net15

# multiplex network with flag not set
net16<-as.networkDynamic(network.initialize(5))
add.edge(net16,tail=1:3,head=4:5)
add.edge(net16,tail=1:3,head=4:5)
nd_test_nets[['multiplex network with flag NOT set']]<-net16

# loops network with flag set
net17<-as.networkDynamic(network.initialize(4,loops=TRUE))
add.edges(net17,tail=1:4,head=1:4)
nd_test_nets[['has loops']]<-net17

# loops network with flag not set
net18<-as.networkDynamic(network.initialize(4))
add.edges(net18,tail=1:4,head=1:4)
nd_test_nets[['has loops but flag NOT set']]<-net18

# bipartite network
net19<-as.networkDynamic(network.initialize(10,bipartite=5))
add.edges(net19,tail=1:5,head=6:10)
nd_test_nets[['bipartite network']]<-net19


# undirected network
net20<-as.networkDynamic(network.initialize(10,directed=FALSE))
add.edges(net20,tail=6:10,head=1:5)
nd_test_nets[['undirected tail > head']]<-net20

# network with net.obs.period having narrower range than data
net21<-network.initialize(10)
add.edges.active(net21,tail=1:9,head=2:10,onset=1:9,terminus=2:10)
net21%n%'net.obs.period'<-list(observations=list(c(3,7)),mode="discrete", time.increment=1,time.unit="step")
nd_test_nets[['net.obs.period having narrower range than data']]<-net21

# network with vertex pid set
net22<-network.initialize(10)
add.edges.active(net22,tail=1:9,head=2:10,onset=1:9,terminus=2:10)
net22%n%'vertex.pid'<-'vertex.names'
nd_test_nets[['vertex.pid set']]<-net22

# network with edge pid set
net23<-network.initialize(10)
add.edges.active(net23,tail=1:9,head=2:10,onset=1:9,terminus=2:10)
set.edge.attribute(net23,'pid',1:9)
net23%n%'edge.pid'<-'pid'
nd_test_nets[['edge.pid set']]<-net23

# network with non-numeric vertex and edge pids
net24<-network.initialize(10)
add.edges.active(net24,tail=1:9,head=2:10,onset=1:9,terminus=2:10)
initialize.pids(net24)
nd_test_nets[['non-numeric vertex and edge pids']]<-net24


# small tree network

# small DAG network

# network with directed cycle

# network with two components

save(nd_test_nets,file='../networkDynamic/data/nd_test_nets.rda')






