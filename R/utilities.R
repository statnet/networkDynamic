#  File networkDynamic/R/utilities.R
#  Part of the statnet package, http://statnetproject.org
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) in
#    http://statnetproject.org/attribution
#
#  Copyright 2012,2013 the statnet development team

################################################
# utilities.R
# Author: Zack W. Almquist, Pavel , Li Wang lxwang@uw.edu, skyebend@uw.edu
#
#
################################################



# see https://statnet.csde.washington.edu/trac/wiki/NetworkDynamicConverterFunctions
# for full specs on this constructor function

networkDynamic <- function(base.net=NULL,edge.toggles=NULL,vertex.toggles=NULL,
                  edge.spells=NULL,vertex.spells=NULL,edge.changes=NULL,vertex.changes=NULL,
                  network.list=NULL,onsets=NULL,termini=NULL,vertex.pid=NULL,start=NULL,end=NULL,net.obs.period=NULL,verbose=TRUE,...) {
  
  
  if (!is.null(start) && !is.null(end)) {
    if (start > end) stop ("start must be less than end")
  }
  if (!is.null(start) || !is.null(end)) {
    if (!is.null(net.obs.period)) stop("net.obs.period can not be specified with start and end arguments")
  }
  if (!is.null(base.net)) {
    if (!is.network(base.net)) stop("base.net must be either NULL or a network object")
  }
  
  if (is.null(network.list) & (!is.null(onsets) || !is.null(termini))) {
    warning("Onsets and termini arguments only used when a network.list is provided.")
  }
  
  # ------- network.list specified ---------
  if (!is.null(network.list)) {
    # ---- check parameters ----
    if (!is.null(vertex.toggles) || !is.null(vertex.spells) ||!is.null(vertex.changes) ||
      !is.null(edge.toggles) || !is.null(edge.spells) ||!is.null(edge.changes)) {
      stop("Vertex or edge activation arguments can not be used along with a network.list")
    }
    
    if (!is.null(onsets)) {
      if (!(is.numeric(onsets) && (length(onsets) == length(network.list)))) {
        stop("Onsets and termini must be NULL or numeric, and the same length as network.list")
      }
    }
    if (!is.null(termini)) {
      if (!(is.numeric(termini) && (length(termini) == length(network.list)))) {
        stop("Onsets and termini must be NULL or numeric, and the same length as network.list")
      }
    }
    
    net.sizes = sapply(network.list, function(y){if (is.network(y)) network.size(y) else NA})
    if (sum(sapply(net.sizes, is.na)) > 0) {
      stop("All elements of network.list must be network objects")
    }
    
    # check network attributes in case they are not the same on all networks
    # this only checks non-user attributes
    net.attribs <- net.attribs <- c('directed', 'hyper', 'loops', 'multiple', 'bipartite')
    temp <- do.call(rbind, lapply(network.list, 
                                 function(x) {
                                    sapply(net.attribs, function(a) get.network.attribute(x, a))}))
    if (!all(sapply(seq_len(ncol(temp)), function(x) length(unique(temp[,x])))==1))
      warning("Networks in network.list have different network properties. Only the first network's attributes are retained.")
    
    if (length(unique(net.sizes)) != 1) {
      if (is.null(vertex.pid)) {
        stop("vertex.pid must be specified when network sizes of network.list are different")
      }
    }
    
    if (!is.null(vertex.pid)) {
      if (vertex.pid %in% list.vertex.attributes(network.list[[1]])) {
        # placeholder
      } else {
        stop("vertex.pid must be present in the network vertex attributes")
      }
    }
    
    # observation period
    if (!is.null(start) && !is.null(onsets)) {
      stop("only one of start and onsets should be specified")
    }
    
    if (is.null(net.obs.period)) {
      # end is not used. if start is specified, assume end to be start + length
      
      if (is.null(start) && is.null(onsets)) {
        start <- 0;
        if (verbose){
          cat("Neither start or onsets specified, assuming start=0\n")
        }
      }
      # only start
      if (is.null(onsets)) {
        net.obs.period <- list(observations=list(c(start,start+length(network.list))),mode="discrete", time.increment=1,time.unit="step")
        onsets<-seq(from=start, length=length(network.list))
        termini<-seq(from=start, length=length(network.list)) + 1
        if (verbose){
          cat("Onsets and termini not specified, assuming each network in network.list should have a discrete spell of length 1\n")
        }
      } else {
        # only onsets
        if (is.null(termini)) stop("onsets and termini must be specified together")
        obs <- lapply(1:length(termini), function(i) c(onsets[i], termini[i]))
        net.obs.period <- list(observations=obs, mode='continuous',time.increment=NA,time.unit='unknown')
      }
        
    } 
    
    # construct the list of vertex attributes that may become TEAs
    TEAvertAttrs<-unique(unlist(lapply(network.list,list.vertex.attributes)))
    TEAvertAttrs<-TEAvertAttrs[!TEAvertAttrs%in%c('na','vertex.names',vertex.pid)]
    # also for edges
    TEAedgeAttrs<-unique(unlist(lapply(network.list,list.edge.attributes)))
    TEAedgeAttrs<-TEAedgeAttrs[!TEAedgeAttrs%in%c('na')]
    # and for networks
    TEAnetAttrs<-unique(unlist(lapply(network.list,list.network.attributes)))
    TEAnetAttrs<-TEAnetAttrs[!TEAnetAttrs%in%c('directed', 'hyper', 'loops', 'multiple', 'bipartite','mnext','net.obs.period','vertex.pid','edge.pid','n')]
       
    
    # ---- vertex.pid present ----
    if (!is.null(vertex.pid)) {
      # build base network vertices <-> pids
      base.net.pids <- NULL      
      for (i in seq_along(network.list)) {
        if (vertex.pid %in% list.vertex.attributes(network.list[[i]])) {
          net.pids <- get.vertex.attribute(network.list[[i]], vertex.pid)
          if (!is.unique.list(net.pids)) 
            stop("vertex.pid attribute must be unique for each vertex.")
          base.net.pids <- c(base.net.pids, net.pids)
        } else {
          stop("vertex.pid must be present in the network vertex attributes for each network")
        }
      }  
      base.net.pids <- sort(unique(base.net.pids))
      
      # initialize, copy network attributes
      vattrs=character(0)
      nattrs=character(0)
      if (is.null(base.net)){ 
        base.net <- network.list[[1]]
        if (verbose){
          cat("Argument base.net not specified, using first element of network.list instead\n")
        }
        # attributes will not be copied from base net
      } else {
        # get the list of attributes to copy from 
        vattrs<-list.vertex.attributes(base.net)
        # don't create a TEA if attribute of the same name exists only in base net
        TEAvertAttrs<-setdiff(TEAvertAttrs,vattrs)
        
        nattrs<-list.network.attributes(base.net)
        nattrs<-setdiff(nattrs,c("bipartite","directed","hyper","loops","mnext","multiple","n"))
      }
      # todo: should we warn here that used first network as base net?
      # I guess the reason to init instead of copy is in case the pids imply a different network size?
      out.net <- network.initialize(length(base.net.pids), directed = base.net%n%"directed", 
                                   hyper = base.net%n%"hyper", loops = base.net%n%"loops", 
                                   multiple = base.net%n%"multiple", bipartite = base.net%n%"bipartite")
      if (verbose){
        cat(paste("Initialized network of size",network.size(out.net),"inferred from number of unique vertex.pids\n"))
      }
      # copy vertex attributes (only occurs if real base net specified)
      
      for(attr in vattrs){
        set.vertex.attribute(out.net,attr,get.vertex.attribute(base.net,attr))
      }
      # copy network attributes EXCEPT network properties (if real base.net specified)
      for (attr in nattrs){
        set.network.attribute(out.net,attr,get.network.attribute(base.net,attr))
      }
    
      # store the vertex.pids
      set.vertex.attribute(out.net, vertex.pid, base.net.pids)
      # store the name of the vertex.pid for later use
      set.network.attribute(out.net,'vertex.pid',vertex.pid)
      
      base.net <- out.net
      
      # get combined edge list, indexed by the vertices in base.net
      for (i in seq_along(network.list)) {
        edgelist <- as.edgelist(network.list[[i]])
        net.pids <- get.vertex.attribute(network.list[[i]], vertex.pid)
        # convert the network vertex indices to base.net indices, using pids
        edges <- apply(edgelist, c(1,2), function(x) {
          which(base.net.pids == net.pids[x])[1]
        })
        
        # activate the vertices
        vertices<-match(net.pids, base.net.pids)
        os<-rep(onsets[i], length(vertices))
        ts<-rep(termini[i], length(vertices))
        activate.vertices(base.net,onset=os,terminus=ts,v=vertices)
        
        # copy any non-standard, vertex, attributes into TEAs
        for(attr in TEAvertAttrs){
          activate.vertex.attribute(base.net,attr,get.vertex.attribute(network.list[[i]],attr,unlist=FALSE),onset=onsets[i],terminus=termini[i],v=get.vertex.id(base.net,net.pids))
        }
        
        # activate the edges
        for (e in seq_len(nrow(edges))) {
          t <- edges[e,1]
          h <- edges[e,2]
          # add edge if necessary
          if (length(get.edgeIDs(base.net, t, h)) == 0) {
            add.edge(base.net, tail=t, head=h)
          }
          eid<-get.edgeIDs(base.net,t,h)[1]
          activate.edges(base.net, e = eid,  onset=onsets[i], terminus=termini[i])
          #  copy any non-standard edge attributes into TEAs
          for(attr in TEAedgeAttrs){
            activate.edge.attribute(base.net,attr,get.edge.attribute(network.list[[i]],attrname=attr,unlist=FALSE)[get.edgeIDs(network.list[[i]],v=edgelist[e,1],alter=edgelist[e,2])],e=eid,onset=onsets[i],terminus=termini[i])
          }
          
        } # end edge copy loop
        
        # copy any non-standard network attributes into TEAs
        for(attr in TEAnetAttrs){
          activate.network.attribute(base.net,attr,get.network.attribute(network.list[[i]],attr,unlist=FALSE),onset=onsets[i],terminus=termini[i])
        }
        
      } # end network loop
      
      
      
      
      
    } else {
      # ---- no vertex.pid, all networks same size ----
      
      # initialize, copy network attributes
      if (is.null(base.net)) base.net <- network.list[[1]]
      if (verbose){
        cat("Argument base.net not specified, using first element of network.list instead\n")
      }
      out.net <- network.initialize(network.size(base.net), directed = base.net%n%"directed", 
                                   hyper = base.net%n%"hyper", loops = base.net%n%"loops", 
                                   multiple = base.net%n%"multiple", bipartite = base.net%n%"bipartite")
      # copy vertex attributes  These may be overwritten by TEAs if they appear on later networks
      vattrs<-list.vertex.attributes(base.net)
      for(attr in vattrs){
        set.vertex.attribute(out.net,attr,get.vertex.attribute(base.net,attr))
      }
      # copy network attributes
      nattrs<-list.network.attributes(base.net)
      nattrs<-setdiff(nattrs,c("bipartite","directed","hyper","loops","mnext","multiple","n"))
      for (attr in nattrs){
        set.network.attribute(out.net,attr,get.network.attribute(base.net,attr))
      }
      base.net <- out.net
      
      # get combined edge list, indexed by the vertices in base.net
      for (i in seq_along(network.list)) {
        edges <- as.edgelist(network.list[[i]])
        
        # all vertices are assumed to be active
        activate.vertices(base.net, onset=onsets[i], terminus=termini[i])
        
        # copy any vertex attributes into a vertex TEA
        for(attr in TEAvertAttrs){
          activate.vertex.attribute(base.net,attr,get.vertex.attribute(network.list[[i]],attr,unlist=FALSE),onset=onsets[i],terminus=termini[i])
        }
        
        # activate the edges
        for (e in seq_len(nrow(edges))) {
          t <- edges[e,1]
          h <- edges[e,2]
          # add edge if necessary
          if (length(get.edgeIDs(base.net, t, h)) == 0) {
            add.edge(base.net, tail=t, head=h)
          }
          eid <- get.edgeIDs(base.net,t,h)[1]
          activate.edges(base.net, e = eid, onset=onsets[i], terminus=termini[i]) 
          # TODO: copy any edge attributes into an edge TEA
          for(attr in TEAedgeAttrs){
            activate.edge.attribute(base.net,attr,get.edge.attribute(network.list[[i]],attrname=attr,unlist=FALSE)[get.edgeIDs(network.list[[i]],v=t,alter=h)],e=eid,onset=onsets[i],terminus=termini[i])
          }
          
        }
        # copy any non-standard network attributes into TEAs
        for(attr in TEAnetAttrs){
          activate.network.attribute(base.net,attr,get.network.attribute(network.list[[i]],attr,unlist=FALSE),onset=onsets[i],terminus=termini[i])
        }
        
      }
      
      
      
    }
    ## check that net.obs.period has appropriate structure
    .check.net.obs.period(net.obs.period)
    if (verbose){
      cat("Created net.obs.period to describe network\n")
      cat(.print.net.obs.period(net.obs.period))
    }
    set.network.attribute(base.net, "net.obs.period", net.obs.period)
    
  } else {  # end network list block
    #todo: if vertex.pid is not null, should we store it? (as a way of setting vertex pid)
    
    # ---------- edge or vertex timings specified  -----------
    # ---- check parameters ----
    if (!is.null(network.list)) {
      stop("Vertex or edge activation arguments can not be used along with a network.list")
    }
    vertex.args <- list(vertex.toggles, vertex.changes, vertex.spells)
    vertex.which <- which(!sapply(vertex.args, is.null))
    if (length(vertex.which) > 1) 
      stop("Only one of vertex.toggles, vertex.spells and vertex.changes should be specified.")
    # pick out the non-null argument, if it is given
    vertex.data <- (if (length(vertex.which)==1) vertex.args[[vertex.which]] else NULL)
  
    
    if (!is.null(vertex.data)){
      # try to convert it if it is not a matrix or a data.frame
      # (don't want to convert data.frame to matrix if we can help it because may force numerics into chars)
      if(!is.data.frame(vertex.data) & !is.matrix(vertex.data)){
        vertex.data <- as.matrix(vertex.data)
      }
      
      if (!is.null(vertex.changes)) {
        if (ncol(vertex.data) < 3) stop("vertex.changes requires 3 columns: time, vertex.id, direction")
        cnames<-colnames(vertex.data,do.NULL=FALSE) # avoid problem when no names exist
        cnames[1:3]<- c("time", "vertex.id", "direction")
        colnames(vertex.data)<-cnames
        if(!is.numeric(vertex.data[,'time'])){
          stop("vertex.changes requires the time column to be numeric")
        }
        if(!is.numeric(vertex.data[,'vertex.id'])){
          stop("vertex.changes requires the vertex.id column to be numeric")
        }
        if(!is.numeric(vertex.data[,'direction'])){
          stop("vertex.changes requires the direction column to be numeric")
        }
      }
      if (!is.null(vertex.toggles) ) {
        if (ncol(vertex.data) < 2) stop("vertex.toggles requires 2 columns: time, vertex.id")
        cnames<-colnames(vertex.data,do.NULL=FALSE)
        cnames[1:2] <- c("time", 'vertex.id')
        colnames(vertex.data)<-cnames
        if(!is.numeric(vertex.data[,'time'])){
          stop("vertex.toggles requires the time column to be numeric")
        }
        if(!is.numeric(vertex.data[,'vertex.id'])){
          stop("vertex.toggles requires the vertex.id column to be numeric")
        }
      }
      
      if (!is.null(vertex.spells)) {
        if (ncol(vertex.data) < 3) stop("vertex.spells requires 3 columns: onset, terminus, vertex.id") 
        cnames<-colnames(vertex.data,do.NULL=FALSE)
        cnames[1:3] <- c('onset', 'terminus', 'vertex.id')
        colnames(vertex.data)<-cnames
        
        if(!is.numeric(vertex.data[,'vertex.id'])){
          stop("vertex.spells requires the vertex.id column to be numeric")
        }
        if(!is.numeric(vertex.data[,'onset'])){
          stop("vertex.spells requires the onset time column to be numeric")
        }
        if(!is.numeric(vertex.data[,'terminus'])){
          stop("vertex.spells requires the terminus time column to be numeric")
        }
      }
      
    }
        
    edge.args <- list(edge.toggles, edge.changes, edge.spells)
    edge.which <- which(!sapply(edge.args, is.null))
    if (length(edge.which) > 1)
      stop("Only one of edge.toggles, edge.spells and edge.changes should be specified.")
    edge.data <- (if (length(edge.which)==1) edge.args[[edge.which]] else NULL)
    
    if (!is.null(edge.data)) {
      
      if(!is.data.frame(edge.data) & !is.matrix(edge.data)){
        # don't want to cast to matrix if it is data frame 'cause may mangle numeric columns
        edge.data <- as.matrix(edge.data)
      }
      if (!is.null(edge.changes)) {
        if (ncol(edge.data) < 4) stop("edge.changes requires 4 columns: time, tail, head, direction")
        cnames<-colnames(edge.data,do.NULL=FALSE)
        cnames[1:4] <- c('time', 'tail', 'head', 'direction')
        colnames(edge.data)<-cnames
        if (!is.numeric(edge.data[,'time'])){
          stop('the time column of the edge.changes argument to networkDynamic must be numeric')
        }
        if (!is.numeric(edge.data[,'tail'])){
          stop('the tail column of the edge.changes argument to networkDynamic must be a numeric vertex id')
        }
        if (!is.numeric(edge.data[,'head'])){
          stop('the head column of the edge.changes argument to networkDynamic must be a numeric vertex id')
        }
        if (!is.numeric(edge.data[,'direction'])){
          stop('the direction column of the edge.changes argument to networkDynamic must be numeric')
        }
      }
      if (!is.null(edge.toggles)) {
        if (ncol(edge.toggles) < 3) stop("edge.toggles requires 3 columns: time, tail, head")
        cnames<-colnames(edge.data,do.NULL=FALSE)
        cnames[1:3] <- c('time', 'tail', 'head')
        colnames(edge.data)<-cnames
        if (!is.numeric(edge.data[,'time'])){
          stop('the time column of the edge.toggles argument to networkDynamic must be numeric')
        }
        if (!is.numeric(edge.data[,'tail'])){
          stop('the tail column of the edge.toggles argument to networkDynamic must be a numeric vertex id')
        }
        if (!is.numeric(edge.data[,'head'])){
          stop('the head column of the edge.toggles argument to networkDynamic must be a numeric vertex id')
        }
      }
      if (!is.null(edge.spells)) {
        if (ncol(edge.spells) < 4) stop("edge.spells requires 4 columns: onset, terminus, tail, head")
        cnames<-colnames(edge.data,do.NULL=FALSE)
        cnames[1:4] <- c('onset', 'terminus', 'tail', 'head')
        colnames(edge.data)<-cnames
        if (!is.numeric(edge.data[,'onset'])){
          stop('the onset time column of the edge.spells argument to networkDynamic must be numeric')
        }
        if (!is.numeric(edge.data[,'terminus'])){
          stop('the terminus time column of the edge.spells argument to networkDynamic must be numeric')
        }
        if (!is.numeric(edge.data[,'tail'])){
          stop('the tail column of the edge.spells argument to networkDynamic must be a numeric vertex id')
        }
        if (!is.numeric(edge.data[,'head'])){
          stop('the head column of the edge.spells argument to networkDynamic must be a numeric vertex id')
        }
      }
    } # end edge data preformatting
      
  
    
    if (is.null(edge.data) && is.null(vertex.data)) warning('neither edge or vertex data were included for network construction')
    
    # ---- initialize base.net ----
    # fill in base network if it is not given
    max.vertex <- max(vertex.data[,'vertex.id'], edge.data[,'tail'], edge.data[,'head'],0)
    if (is.null(base.net)){
      if (verbose){
        cat(paste("Initializing base.net of size",max.vertex,"imputed from maximum vertex id in edge records\n"))
      }
      base.net <- network.initialize(max.vertex)
    } 
    
    if (is.null(net.obs.period)) {
      # observation.period and censoring
      
      # default to max and min of time range
      if (is.null(start)){
        start <- mintime(vertex.data, edge.data)
      }
      if (is.null(end)){
        end <- maxtime(vertex.data, edge.data)
      }
      net.obs.period <- list(observations=list(c(start, end)))
      if (!is.null(edge.spells) || !is.null(vertex.spells)) {
        net.obs.period$mode <- 'continuous'
        net.obs.period$time.increment<-NA
        net.obs.period$time.unit<-"unknown"
      } else {
        net.obs.period$mode <- 'discrete'
        net.obs.period$time.increment<-1
        net.obs.period$time.unit<-"step"
        
      }
    } else {
      if (!is.null(start)) stop("start and end arguments should not be specified with net.obs.period argument")
    }
    # verify that net.obs.period has good structure
    .check.net.obs.period(net.obs.period)
    if (verbose){
      cat("Created net.obs.period to describe network\n")
      cat(.print.net.obs.period(net.obs.period))
    }
    set.network.attribute(base.net, "net.obs.period", net.obs.period)
    
    # strict construction for now
    if (max.vertex > network.size(base.net)) stop("base.net network size is smaller than size implied by vertex.ids in vertex or edge argument")
    
    # remove any activity from base.net (for now)
    delete.vertex.activity(base.net)
    if (network.edgecount(base.net) > 0){
      delete.edge.activity(base.net)
      cat("Edge activity in base.net was ignored\n")
    }
    
    
    # ---- vertex data ----
    if (!is.null(vertex.data)){
      # sort by time
      vertex.data <- vertex.data[order(vertex.data[,1,drop=FALSE]), ,drop=FALSE]
      
      # initialize
      if (!is.null(vertex.toggles)) activate.vertices(base.net, onset=-Inf, terminus=Inf)
      
      if (!is.null(vertex.spells)) {
        activate.vertices(base.net, v=vertex.data[,'vertex.id'], onset=vertex.data[,'onset'], terminus=vertex.data[,'terminus'])
      } else {
        for (i in seq_len(nrow(vertex.data))) {
          # todo: maybe do this in try catch so we can give appropriate line numbers for errors?
          at <- vertex.data[i,'time']
          v <- vertex.data[i,'vertex.id']
          change.activate <- (if (is.null(vertex.changes)) !is.active(base.net, at=at, v=v) else vertex.data[i,'direction']==1) 
          if (change.activate) {
            activate.vertices(base.net, v=v, onset=at, terminus=Inf)
          } else {
            deactivate.vertices(base.net, v=v, onset=at, terminus=Inf)
          }
        }
      }
    }
    
    # ---- edge data ----
    if (!is.null(edge.data)) {        
      # sort by onset time
      edge.data <- edge.data[order(edge.data[,1]), ,drop=FALSE]
      
      # initialize
      #if (is.null(edge.spells)) activate.edges(base.net, onset=-Inf, terminus=Inf)
      
      #if we are in the spells case, and no edges exist yet we can avoid actually looping
      if (!is.null(edge.spells)){
        dyads<-unique(edge.data[,3:4,drop=FALSE])
        tails<-as.list(dyads[,1])
        heads<-as.list(dyads[,2])
        add.edges(base.net,tail=tails,head=heads)
        eids<-sapply(seq_len(nrow(edge.data)),function(i){get.edgeIDs(base.net,v=edge.data[i,3],alter=edge.data[i,4])})
        if (length(eids)>0){
          activate.edges(base.net,onset=edge.data[,1],terminus=edge.data[,2],e=eids)
        }
  
      } else {  # changes or toggles, so have to loop to avoid hurting our heads
        # assume edges in base.net to be active initially
        if (!is.null(edge.toggles)) activate.edges(base.net, onset=-Inf, terminus=Inf)
        for (i in seq_len(nrow(edge.data))) {
          t <- edge.data[i,2] 
          h <- edge.data[i,3]
          e <- get.edgeIDs(base.net, t, h)
          # add edge if not present in the base.net (as inactive?)
          # TODO: problem here with directed vs undirected networks?
          # TODO: how to handle multiplex/duplicate edge case?
          if (length(e) == 0) {
            add.edge(base.net, t, h)
            e <- get.edgeIDs(base.net, t, h)
            if (!is.null(edge.toggles)) deactivate.edges(base.net, e=e, onset=-Inf, terminus=Inf)
          }
          at <- edge.data[i,'time']
          change.activate <- (if (!is.null(edge.toggles)) !is.active(base.net, at=at, e=e) else edge.data[i,'direction']==1) 
          if (change.activate) {
            activate.edges(base.net, e=e, onset=at, terminus=Inf)
          } else {
            deactivate.edges(base.net, e=e, onset=at, terminus=Inf)
          }
          
        }
      } # end of non-spell edge creation
    } # end edge data
    
  } # end non-network.list part
  
  # if only base net is specified, set.nD.class on it and return. 
  return(set.nD.class(base.net))
  
  
  # temporariy kludge, call other hidden package functions
  #  if (!is.null(edge.toggles) & !is.null(base.net)){
  #    if (is.null(start)){
  #      start<-min(edge.toggles[,1])-1
  #    }
  #    if (is.null(end)){
  #      end<-max(edge.toggles[,1])
  #    }
  #    return (as.networkDynamic.network(base.net,toggles=edge.toggles,start=start,end=end))
  #    
  #  }
  
  
}

################
### start networkDynamic-> other formats
################

# Get activity functions

# wrapper functions to return activity matrices of edges and vertices
get.edge.activity <- function(x, e=seq_along(x$mel), as.spellList=FALSE,active.default=TRUE) {
  if(length(x$mel)>0) 
    if((min(e) < 1) || (max(e) > x%n%"mnext"-1)) 
      stop("Illegal edge in get.edge.activity.\n")
  
  if (as.spellList) {
    return(as.data.frame.networkDynamic(x, e=e,active.default=active.default))
  } else {
    spls<-get.edge.attribute(x$mel, "active", unlist=FALSE)
    if (active.default & length(e)>0){
      # hard to distinguish between edges that are missing one ones with no activity
      # figure out which e correspond to deleted edges and get rid of 'em
      eExist<-which(!sapply(x$mel,is.null))
      # replace spells that exist and are null
      spls[intersect(eExist,which(sapply(spls,is.null)))]<-list(matrix(c(-Inf,Inf),ncol=2))
    }
    # also remove any 'null spells' (Inf,Inf)
    spls<-.removeNullSpells(spls)
    #trim to e
    spls<-spls[e]
    return(spls)
  }
  
}

get.vertex.activity <- function(x, v=seq_len(network.size(x)), as.spellList=FALSE,active.default=TRUE) {
  if((min(v,Inf) < 1) || (max(v,-Inf) > network.size(x))) 
    stop("Illegal vertex in get.vertex.activity.\n")  
  
  if (as.spellList) {
    return(get.vertex.spelllist(x, v=v,active.default=active.default))
  } else {
    vam=get.vertex.attribute(x, "active", unlist=FALSE)
    vam<-vam[v]
    if(length(vam)>0){
      # if active default, need to add spells for vertices with no spells
      if (active.default ){
        vam[is.na(vam)]<-list(matrix(c(-Inf,Inf),ncol=2))
      }
      # also 'remove any' 'null spells' (Inf,Inf)
      # don't actually remove them, because then would lose order of v
      vam<-.removeNullSpells(vam)
    } else { # no vertices case
      vam<-list()
    }
  }
  return(vam)
}



# tail and head are nodeIDs
as.data.frame.networkDynamic<-function(x, row.names = NULL, optional = FALSE,e=seq_along(x$mel), start=NULL, end=NULL, active.default=TRUE,...){
  if(is.null(start) && !is.null(attr(x,"start"))) start <- attr(x,"start")
  if(is.null(end) && !is.null(attr(x,"end"))) end <- attr(x,"end")
  
  # deprecation warning because we want to use net.obs.period instead of invisible attrs
  if( !is.null(attr(x,"start")) |  !is.null(attr(x,"end"))){
    .Deprecated(msg='Indicating the network activity bounds for censoring with attrs for "start" and "end" attached to a network object has been deprecated. \nPlease use the network-level attribute "net.obs.period" instead.  \nSee ?net.obs.period for more information' )
  }

  # check if net.obs.period is present
  # use max and min for censoring bounds if not included.
  if(!is.null(x%n%'net.obs.period')){
    if(is.null(start)){
      start<-min(unlist((x%n%'net.obs.period')$observations))
    }
    if(is.null(end)){
      end<-max(unlist((x%n%'net.obs.period')$observations))
    }
  }
  
  tm<-lapply(seq_along(x$mel),function(y){
    edge<-x$mel[[y]]
    if(is.null(edge)) NULL else{
      active<-edge$atl$active
      if (!is.null(active)) {
        ac<-matrix(rep(cbind(edge$outl,edge$inl,y),nrow(active)),ncol=3,byrow=TRUE)
        cbind(active,ac)
      } else {
        # activity attribute is missing, so use active default 
        if(active.default){
          # create -Inf Inf spell here
          matrix(c(-Inf,Inf,edge$outl,edge$inl,y),ncol=5,byrow=TRUE)
        }
      }
    }
  })
  out <- do.call(rbind,tm)
  if (is.null(out)) {
    out = data.frame(onset=numeric(), terminus=numeric(), tail=numeric(), head=numeric(),edge.id=numeric())
    warning("Network does not have any edge activity")
  } else {
    colnames(out)<-c("onset","terminus","tail","head","edge.id")
  }
  out<-data.frame(out)
  
  #remove any rows with 'null' (Inf,Inf) spells
  out<-out[!(out[,1]==Inf&out[,2]==Inf),]
  
  # do censoring
  out$onset.censored <- out$onset==-Inf
  out$terminus.censored <- out$terminus==Inf
  
  if(!is.null(start)) out$onset[out$onset.censored] <- start
  
  if(!is.null(end)) out$terminus[out$terminus.censored] <- end
  
  out$duration <- out$terminus-out$onset  
  
  # have to permute columns to put edge.id at end
  out<-out[,c(1,2,3,4,6,7,8,5)]
  
  # remove any edges not specified in original arugments TODO: why not do this at beginning?
  out <- out[as.numeric(out$edge.id)%in%e,]
  # out <- out[order(out$onset),]
  # sort output by eid, onset,terminus
  out<-out[order(out[,8],out[,1],out[,2]),]
  
  return(out)
}

# return [onset, terminus, vertex]
get.vertex.spelllist = function (x, v=seq.int(network.size(x)), start=NULL, end=NULL,active.default=TRUE) {
  # todo: need to replace this with reading net.obs.period attribute
  if(is.null(start) && !is.null(attr(x,"start"))) start <- attr(x,"start")
  if(is.null(end) && !is.null(attr(x,"end"))) end <- attr(x,"end")
  
  # deprecation warning because we want to use net.obs.period instead of invisible attrs
  if( !is.null(attr(x,"start")) |  !is.null(attr(x,"end"))){
    .Deprecated(msg='Indicating the network activity bounds for censoring with attrs for "start" and "end" attached to a network object has been deprecated. \nPlease use the network-level attribute "net.obs.period" instead.  \nSee ?net.obs.period for more information' )
  }
  
  # check if net.obs.period is present
  # use max and min for censoring bounds if not included.
  if(!is.null(x%n%'net.obs.period')){
    if(is.null(start)){
      start<-min(unlist((x%n%'net.obs.period')$observations))
    }
    if(is.null(end)){
      end<-max(unlist((x%n%'net.obs.period')$observations))
    }
  }
  
  # grab the list of spells
  node.list <- get.vertex.activity(x,v=v,active.default=active.default)
  # deal with NA or nullcaused by vertices w/o spells defined
  if (active.default & length(node.list)>0){
    node.list[is.na(node.list)]<-list(insert.spell(NULL,onset=-Inf,terminus=Inf)) # always active
    node.list[is.null(node.list)]<-list(insert.spell(NULL,onset=-Inf,terminus=Inf)) # always active
  } 
  # don't include spell if null or na
  v<-v[!is.na(node.list)]
  node.list<-node.list[!is.na(node.list)]
  v<-v[!sapply(node.list,is.null)]
  node.list<-node.list[!sapply(node.list,is.null)]
  
  out <- lapply(seq_along(v), function(i){cbind(node.list[[i]][,1], node.list[[i]][,2],v[i])})
  out <- do.call(rbind, out)
  
  if (is.null(out)) {
    out <- data.frame(onset=numeric(), terminus=numeric(), vertex.id=numeric())
  } else {
    colnames(out) <- c("onset", "terminus", "vertex.id")
  }
  
  out <- data.frame(out)
  
  # figure out censoring
  out$onset.censored <- out$onset==-Inf
  out$terminus.censored <- out$terminus==Inf
  
  if(!is.null(start)) out$onset[out$onset.censored] <- start
  
  if(!is.null(end)) out$terminus[out$terminus.censored] <- end
  # figure out duration
  out$duration <- out$terminus-out$onset  
  
  # impose sort order
  #out <- out[order(out$onset, out$vertex),]
  out <- out[order(out$vertex.id, out$onset,out$terminus),]
  out
}

################
### end networkDynamic-> other formats
################

print.networkDynamic <- function(x, ...){
  cat("NetworkDynamic properties:\n")
  times <- get.change.times(x,ignore.inf=FALSE)
  if (length(times)==0){
    cat("  network contains no time information")
  } else {
    cat("  distinct change times:", length(times), "\n")
    maxrange<-range(times)
    cat("  maximal time range:", maxrange[1], "to",maxrange[2],"\n")
  }
  # TEAs
  ntea <-list.network.attributes.active(x,onset=-Inf,terminus=Inf,dynamic.only=TRUE)
  if (network.size(x)>0){
    vtea <-list.vertex.attributes.active(x,onset=-Inf,terminus=Inf,dynamic.only=TRUE)
    etea <-list.edge.attributes.active(x,onset=-Inf,terminus=Inf,dynamic.only=TRUE)
  } else {
    vtea<-character(0)
    etea<-character(0)
  }
  if (length(ntea)+length(vtea)+length(etea)>0){
    cat("\n Dynamic (TEA) attributes:\n")
    if (length(ntea)>0){
      cat("  Network TEAs:")
      cat(paste("   ",ntea,"\n"))
    }
    if (length(vtea)>0){
      cat("  Vertex TEAs:")
      cat(paste("   ",vtea,"\n"))
    }
    if (length(etea)>0){
      cat("  Edge TEAs:")
      cat(paste("   ",etea,"\n"))
    }
  }
  
  # report on spell ranges for tea attributes?
  
  if ('net.obs.period'%in%list.network.attributes(x)){
    cat("\nIncludes optional net.obs.period attribute:\n")
    .print.net.obs.period(get.network.attribute(x,'net.obs.period',unlist=FALSE))
  }
  # print standard network stuff
  # todo: should we override print.network so we can supress printing of net.obs.period and info on TEAs?
  cat("\n")
  NextMethod("print")
}


##############
### as.networkDynamic
### converts various objects to networkDynamic
##############

is.networkDynamic <- function(x){
  "networkDynamic" %in% class(x)
}

as.networkDynamic <- function(object,...){
  UseMethod("as.networkDynamic")
}

# doesn't do anything. Returns object as is
as.networkDynamic.networkDynamic <- function(object,...){
  return(object)
}

# only sets the networkDynamic class
as.networkDynamic.network<- function(object,...){
  set.nD.class(object)
  return(object)
}

# remove networkDynamic class but leave object unchanged
as.network.networkDynamic<-function(x,...){
  if(is.networkDynamic(x)){
    class(x)<-class(x)[class(x)!='networkDynamic']
  }
  return(x)
}


# if x is an nD object, return x
# otherwise, modify it in its parent frame
set.nD.class <- function(x){
  if(!is.networkDynamic(x)) {
    xn <- deparse(substitute(x))
    ev <- parent.frame()
    class(x) <- c("networkDynamic", class(x))
    if(exists(xn, envir=ev))
      on.exit(assign(xn, x, pos=ev))
    return(invisible(x))
  }
  return(x)
}

as.edgelist <- function(nw, attrname = NULL, as.sna.edgelist = FALSE,...){
  el <- as.matrix.network.edgelist(nw, attrname=attrname, as.sna.edgelist=as.sna.edgelist,...)
  if(!is.directed(nw)) el[,1:2] <- cbind(pmin(el[,1],el[,2]),pmax(el[,1],el[,2]))
  el
}

# given a network object (or networkDynamic) and a vertex name, return the vertex id
# net: network or networkDynamic object
# vertex.name: the vertex name to look up id for. Usually string type
# returns the internal id of the vertex name. Gives a warning if the name isn't unique.
# THIS IS REPLACED BY VERSION IN vertex.pid.R
#get.vertex.id = function (net, vertex.name) {
#  if (!is.network(net)) stop("Error: argument is not a network object")
#  temp = which(network.vertex.names(net) == vertex.name)
#  if (length(temp) == 0) stop("Error: vertex name not found")
#  if (length(temp) > 1) warning("Warning: vertex names are not unique!")
#  return(temp[1])
#}

# given a vector or list, return true if it is unique
is.unique.list <- function(x) {
  length(x) == length(unique(x))
}



## returns the min and max times of vertex and edge timings
mintime <- function(vertex.data, edge.data) {
  if ('time' %in% colnames(vertex.data)) t1 = 'time' else t1 = 'onset'
  if ('time' %in% colnames(edge.data)) t2 = 'time' else t2 = 'onset'
  if ((length(vertex.data[,t1]) + length(edge.data[,t2]))==0){
    return(-Inf)
  } else {
    return(min(vertex.data[,t1], edge.data[,t2]))
  }
}

maxtime <- function(vertex.data, edge.data) {
  if ('time' %in% colnames(vertex.data)) t1 = 'time' else t1 = 'terminus'
  if ('time' %in% colnames(edge.data)) t2 = 'time' else t2 = 'terminus'
  if ((length(vertex.data[,t1]) + length(edge.data[,t2]))==0){
    return(Inf)
  } else {
    return(max(vertex.data[,t1], edge.data[,t2]))
  }
}

## checks that net.obs.period object has appropriate structure

.check.net.obs.period <- function(x){
  if (!is.list(x)){
    stop("net.obs.period must be a list object")
  } 
  elements<-names(x)
  if(!'observations'%in%elements){
    stop('net.obs.period must contain a sub-list named "observations" containing at least one spell')
  }
  if (!all(is.numeric(unlist(x$observations)))){
    stop("all elements of the 'observations' component of net.obs.period must be numeric")
  }
  if (!all(sapply(x$observations,length)==2)){
    stop("all elements of the 'observations' component of net.obs.period must be a vector of length 2")
  }
  
  # must be possible to compute a max and min bound
  tryCatch({max(unlist(x$observations))},
           warning=function(w){
             stop("unable to compute a max and min value for the 'observations' component of net.obs.period", 
                  call. = FALSE)
           },
           error=function(e){
             stop("unable to compute a max and min value for the 'observations' component of net.obs.period", 
                  call. = FALSE)
           }
  )         
  
  if(!'mode'%in%elements){
    stop('net.obs.period must contain an element named "mode" indicating the time model')
  }
  if(!'time.increment'%in%elements){
    stop('net.obs.period must contain a element named "time.increment" which gives the natural time increment (but it can have the value NA if does not apply)')
  }
  if(!'time.unit'%in%elements){
    stop('net.obs.period must contain a element named "time.unit" which names the time unit for the network object')
  }
}

# internal function to pretty-print net.obs.period attribute
.print.net.obs.period<-function(nop){
  cat(" Network observation period info:\n")
  cat(paste("  Number of observation spells:",length(nop$observations),"\n"))
  maxrange<-range(nop$observations)
  cat(paste("  Maximal range of observations:",maxrange[1],"to",maxrange[2],"\n"))
  cat(paste("  Temporal mode:",nop$mode,"\n"))
  cat(paste("  Time unit:",nop$time.unit,"\n"))
  cat(paste("  Suggested time increment:",nop$time.increment,"\n"))
}
