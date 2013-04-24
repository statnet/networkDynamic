# functions for reconciling edge and vertex activity



reconcile.vertex.activity<-function(net,mode="match.to.edges",edge.active.default=TRUE){
  if (!is.networkDynamic(net)){
    stop("reconcile.vertex.activity can only be applied to networkDynamic objects")
  }
  xn <- deparse(substitute(net))
  ev <- parent.frame()
  v<-seq_len(network.size(net))
  if(mode=='match.to.edges'){
    # set the activity spells of all vertices to match the activity of their incident edges
    # delete the vertices' activity
    delete.vertex.activity(net,v=v)
    # for each vertex
    for (vert in v) {
      # get the ids of incident edges
      eids<-get.edgeIDs(net,v=vert,neighborhood='combined')
      if (length(eids)>0){
        # get the activity of those edges and  union the edge activiy spells
        activity<-unique(get.edge.activity(net,e=eids,as.spellList=TRUE,active.default=edge.active.default)[,1:2])
        if (nrow(activity)>0){
          # set vertex activity to edges' activity
          activate.vertices(net,v=vert,onset=activity[,1],terminus=activity[,2])
        } else {
          # vert's edges have no valid activity, so deactivate
          deactivate.vertices(net,v=vert)
        }
      } else { 
        # vert has no incident edges, so deactivate
        deactivate.vertices(net,v=vert)
      }
    }
    
  } else {
    stop('reconcile vertex activity mode ', mode, ' not supported')
  }
  if (exists(xn, envir = ev)) 
    on.exit(assign(xn, net, pos = ev))
  invisible(net)
}
