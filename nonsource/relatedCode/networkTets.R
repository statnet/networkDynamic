# tests for networks

mat <- as.data.frame(list(c(1,2,3), c(2,3,4),c(10,11,12)))
net <- network(mat,matrix.type='edgelist',ignore.eval=FALSE,names.eval="myEdges")
net%e%'myEdges'

mat <- as.data.frame(list(c("z","y","x"), c("y","x","w"),c(10,11,12)))
net <- network(mat,matrix.type='edgelist',ignore.eval=FALSE,names.eval="myEdges")
net%e%'myEdges'
as.matrix.network.edgelist(net)

net2 <- network(mat,matrix.type='edgelist')
net2%e%'myEdges'<-mat[,3]
net2%e%'myEdges'
as.matrix.network.edgelist(net2)


mat2 <- as.data.frame(list(c("x","y","z"), c("w","x","y"),c(10,11,12)))
net3 <- network(mat2,matrix.type='edgelist')
net%e%'myEdges'
as.matrix.network.edgelist(net3)

# input order doesn't seem to matter for data.frames
mat[1,1]
as.numeric(mat[1,1])

mat2[3,1]
as.numeric(mat2[3,1])
