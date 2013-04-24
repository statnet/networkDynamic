#  File networkDynamic/R/utilities.R
#  Part of the statnet package, http://statnetproject.org
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) in
#    http://statnetproject.org/attribution
#
#  Copyright 2012 the statnet development team

################################################
# utilities.R
# Author: Zack W. Almquist, Pavel 
#
# Includes:
#		     c.network #overloaded c operator for network objects
#
#	misc helper functions not for general use
#
################################################


# this will be the new wrapper function for converting and mashing things into networkDynamic objects
# 
networkDynamic <-function(base.net=NULL,edge.spells=NULL,vertex.spells=NULL,edge.toggles=NULL,vertex.toggles=NULL,start=NULL,end=NULL,...){
  
  # for now, give warnings if unsupported arguments are used
  
  if (!is.null(vertex.spells)){
    stop("Vertex spell conversion not yet implemented")
  }
  
  if (!is.null(edge.spells)){
    stop("Edge spell conversion not yet implemented")
  }
  
  if (!is.null(vertex.toggles)){
    stop("Vertex toggle conversion not yet implemented")
  }
  
  # if only base net is specified, set.nD.class on it and return. 
  
# temporariy kludge, call other hidden package functions
  if (!is.null(edge.toggles) & !is.null(base.net)){
    if (is.null(start)){
      start<-min(edge.toggles[,1])-1
    }
    if (is.null(end)){
      end<-max(edge.toggles[,1])
    }
    return (as.networkDynamic.network(base.net,toggles=edge.toggles,start=start,end=end))
    
  }
  
}



################
### start networkDynamic-> other formats
################
# tail and head are nodeIDs
as.data.frame.networkDynamic<-function(x,row.names = NULL, optional = FALSE, start=NULL, end=NULL, ...){
  if(is.null(start) && !is.null(attr(x,"start"))) start <- attr(x,"start")
  if(is.null(end) && !is.null(attr(x,"end"))) end <- attr(x,"end")
  
  tm<-lapply(x$mel,function(y){
    if(is.null(y)) NULL else{
      active<-y$atl$active
      if (!is.null(active)) {
        ac<-matrix(rep(cbind(y$outl,y$inl),nrow(active)),ncol=2,byrow=TRUE)
        cbind(active,ac)
      }
    }
  })
  out <- do.call(rbind,tm)
  if (is.null(out)) {
    out = data.frame(onset=numeric(), terminus=numeric(), tail=numeric(), head=numeric())
    warning("Object does not have any edge activities")
  } else {
    colnames(out)<-c("onset","terminus","tail","head")
  }
  out<-data.frame(out)
  
  out$onset.censored <- out$onset==-Inf
  out$terminus.censored <- out$terminus==Inf

  if(!is.null(start)) out$onset[out$onset.censored] <- start
  
  if(!is.null(end)) out$terminus[out$terminus.censored] <- end
  
  out$duration <- out$terminus-out$onset  
  
  # edge ids
  eids = mapen(x,out[,3:4])
  out$edge.id = eids
  
  out
}
################
### end networkDynamic-> other formats
################

print.networkDynamic <- function(x, ...){
  #times <- sort(unique(c(lapply(x$mel, function(e) e$atl$active),recursive=TRUE)))
  #cat("networkDynamic with", length(times), "distinct change times:\n")
  #print(times)
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
  .Deprecated("networkDynamic()",old="as.networkDynamic.network",msg="networkDynamic converter and data merge functions are being refactored to avoid `as.networkDynamic.*' methods and S3 calls.  See networkDynamic function instead. In the future as.networkDynamic will only set a networkDynamic class on its argument.")
  UseMethod("as.networkDynamic")
}

# doesn't do anything. Returns object as is
as.networkDynamic.networkDynamic <- function(object,...){
  return(object)
}

## Converts a network object to a networkDynamic object, given a spell list or a toggle list
# note that it deletes all existing edges in the network object and replaces them with edges from "spells"
# object: network object
# spells: spell list in the format
#     start end tail head left.censored right.censored duration
# toggles: toggle list of the format: time tail head
# start and end: times for the network

as.networkDynamic.network <- function(object, spells=NULL, toggles=NULL, start=NULL, end=NULL,...){
  # what if both are specified?
  if(is.null(spells)){
    if(is.null(toggles)){
      stop("Either spell list or toggle list must be given.")
    }
    toggles <- as.data.frame(toggles)
    spells <- duration.matrix(object, toggles, start, end)
  } else 
    spells <- as.data.frame(spells)
  
  # takes the tail and head columns from the spells to build new edges
  newedges <- as.matrix(spells[,3:4])
  # if edges are not directed, rearrange the edges so head is always smaller than tail
  if(!is.directed(object)) newedges <- cbind(pmin(newedges[,1],newedges[,2]),pmax(newedges[,1],newedges[,2]))
  newedges <- unique(newedges)
  
  nw <- object
  ## delete all the edges from nw. length(nw$mel) gives the largest edgeID in the network
  # original bug: delete.edges(nw)
  delete.edges(nw, seq_along(nw$mel))
  
  # add new edges to the stripped network
  nw <- add.edges(nw,newedges[,1],newedges[,2])
  
  # get the edge id's of the new edges in nw
  men<-mapen(nw,spells[,3:4])
  #colnames(spells)<-c("start","end","tail","head","left.censored","right.censored")
  x<-cbind(men,spells)
  
  # generate the networkDynamic object using nw and the spell list
  out <- networkDynamicInternal(nw,x)
  set.nD.class(out)
}

## converts a data frame of edge spells into a networkDynamic object
# object: dataframe of the format (last 3 are optional)
#   start  end  tail  head  left.censored  right.censored  duration
# nodeInfo: should be a dataframe of the format
#   vertex.id   onset   terminus   NodeId(aka vertex name)
# n: network size (number of vertices)
# the rest are network attributes
# returns a networkDynamic object
as.networkDynamic.data.frame <- function(object,nodeInfo=NULL, n=NULL, directed = TRUE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = FALSE,...){
  # the data frame is the spellInfo
  spellInfo <- as.data.frame(object)
  
  # no nodeInfo specified
  if(is.null(nodeInfo)){
    # is the network size specified?
    if(is.null(n)){
      # scrapes the node IDs (numbers) from tail and head columns in the data frame
      nodeIDs  <- sort(unique(c(spellInfo[,3:4],recursive=TRUE)))
      # takes the max of the nodeIDs as the network size
      # n <- length(nodeIDs)
      n <- max(nodeIDs)
    }else{
      nodeIDs <- 1:n
    }
    # initialize the network based on network size and the parameters
    net <- network.initialize(n,directed=directed,hyper=hyper,loops=loops,multiple=multiple,bipartite=bipartite)
    # note that the nodeIDs may not cover every node in the size-n network, 
    #  if length(nodeIDs) < max(nodeIDs)
    #  so this needs to be fixed
    # net %v% "vertex.names" <- nodeIDs
    net %v% "vertex.names" <- 1:n
  }else{
    # figure out how this works
    nodeInfo <- as.data.frame(nodeInfo)
    net<-networkDynamicInterV(nodeInfo,directed=directed,hyper=hyper,loops=loops,multiple=multiple,bipartite=bipartite)
  }
  
  # name the columns of spellInfo, padding with NA's as necessary
  colnames(spellInfo) <- c("start","end","tail","head","left.censored","right.censored")[1:ncol(spellInfo)]
  # net is a network object, so this calls as.networkDynamic.network with spellInfo
  out <- as.networkDynamic(net, spellInfo)
	
  # if nodeInfo is a network, then add the attributes to out? This is confusing.
	if(is.network(nodeInfo)){
  	if(length(unique(sapply(nodeInfo,network.size)))==1){
  		out<-addAttributes(out,nodeInfo)
  	}
	}
	## Check for vertex dynamics
	## if no vertex dynamics add attributes
	## addAttributes(out,)
	###
  
  set.nD.class(out)
}

##############
### ndConverter.network.list
### converts a list of network objects to networkDynamic and back
### object: network list
##############

as.networkDynamic.network.list<-as.networkDynamic.list<-function(object,...){
	warning("Keeps only attributes on first network! Temporally extended attribute (TEA) methods coming soon.\n")
	if(is.network(object[[1]])){
		out<-listSpell(object,buildNet(object))
		## Check if vertex length is same for all the networks. if yes, copy attributes
    ## This really should have been done with a for loop :P
		if(length(unique(sapply(object,
                            function(y){
                              if(!is.network(y)){return(NULL)}
                              network.size(y)}
                            )
                     )
              )==1) {
        out<-addAttributes(out,object[[1]])}
		return(set.nD.class(out))
		}
	warning("Not a network.list object.\n")
}




###########################################################
### Helper functions, internal functions
### These do not need to exported if a specific NAMESPACE file is
### provided
###########################################################

#### copies attributes over
addAttributes<-function(x,net){
	x%n%"directed"<-net%n%"directed"
	m<-match(x%v%"vertex.names",net%v%"vertex.names")
	attr<-list.vertex.attributes(net)
	for(i in 1:length(attr)){
		out<-set.vertex.attribute(x,attr[i],(net%v%attr[i])[m])
		}
	out
}

### Spell dynamics from vertex list
networkDynamicInterV<-function(x,...){
	warning("All non-unique spells to Vertices dropped")
	nam<-unique(x$NodeId)
	net<-network.initialize(length(nam),...)
	net%v%"vertex.names"<-nam
	net<-activate.vertices(net,onset=x[,2],terminus=x[,3],v=x[,1])	
	### need to manage issue of multiple spells in one vertex
	net
}



##### add spell to bn using x
# x: network list
# bn: base network containing all the vertices and edges from the network list, "flattened" into one network
# returns the base network with spells added
listSpell<-function(x,bn){
  # check if they are networks
	test1<-sapply(x,is.network)
  # check if they have more than one vertex??
	test2<-sapply(x,function(x){ifelse(is.network(x), x%n%"mnext">1, FALSE)})
	for(i in 1:length(x)){
		if(test1[i]){
      if(test2[i]){
        # add the spell from time i-1 to i, based on the list
        # what happens at the beginning?? Starts at t=0, increments by 1 unit time steps
        bn<-addSpellnl(x[[i]],bn,i-1,i)
        }}
		}
	bn
}

### adds spell 
# net: one of the networks in the network list (at time2)
# bn: the base network to add spells to
# time1: range from 0 to n-1, where n is the number of networks in the list
# time2: from 1 to n
# returns
addSpellnl<-function(net,bn,time1,time2){
	### Edge Spell
  # turns the edgelist in net into vertex numbers used in bn
	elt<-mvnum(snel(as.edgelist(net)),bn%v%"vertex.names") ## builds edgelist 
  # gets the edge id's of the edgelist
	eids<-mapen(bn,elt)
  # onset and terminus times for those edges in net
	os<-rep(time1,length(eids))
	ts<-rep(time2,length(eids))
  # activates that edge in bn
	bn<-activate.edges(bn,onset=os,terminus=ts,e=eids)
	
	### Vertex spell
	vid<-match(net%v%"vertex.names",bn%v%"vertex.names")
	os<-rep(time1,length(vid))
	ts<-rep(time2,length(vid))
	bn<-activate.vertices(bn,onset=os,terminus=ts,v=vid)
	bn
}

### builds basis network
# x: network list object
# returns a network object with every vertex and every edge in the network list,
#  ready to accept spells
buildNet<-function(x){
  # get unique list of vertex names from the whole list
  nam<-getNames(x)
  # builds network from the vertex names list with the attributes from the first
  # network in the list
  base<-baseNetwork(nam,x[[1]])
  # gets the combined edge list from all the networks in the list, using the vertex names in nam
	nel<-namedEL(x)
  # m is the combined edge list, with the vertex id's in nam
	m<-mvnum(nel,nam)
  # add the edges to the base network
	base<-add.edges(base,tail=m[,1],head=m[,2])
	base
}

### gets unique namelist
# ln: network.list object
# returns list of unique vertex names as strings, sorted either by numerical order or
#   by string order

getNames<-function(ln){
  # takes the vertex names from each of the networks in the list, concatenates them into
  # a single list, and then removes any duplicates
  out<-unique(unlist(lapply(ln,
            function(x){
              if(is.network(x)){return(x%v%"vertex.names")}
            })))
  opt <- options(warn=-1) # as.numeric produces warnings if not coerceable; turn them off
  # If out can be coerced to numeric without any NAs, then sort it as numeric
  # some of the names may be strings
  if (all(!is.na(tmp<-as.numeric(out)))) {
    out <- tmp
  }
  # what if there are NA's? happens when the name itself is NA
  # then we don't coerce and sort it as the original x%v%'vertex.names' output,
  # which may contain string names
  options(opt)
  as.character(sort(out))
}

#### copies network attributes to empty network
# nam: list of unique vertex names
# x: network to copy attributes from
# returns a network object

baseNetwork<-function(nam,x){
  out<-network.initialize(length(nam), directed = x%n%"directed", hyper = x%n%"hyper", loops = x%n%"loops", multiple = x%n%"multiple", bipartite = x%n%"bipartite")
  out%v%"vertex.names"<-nam
  out
}

### single named edge list
# el: edgelist
# returns the edgelist with the vertex id's replaced by the vertex names
snel<-function(el){cbind(attr(el,"vnames")[el[,1]], attr(el,"vnames")[el[,2]])}


### full named edgelist
# ln: network list
# returns a list of edges (as matrix), labeled by vertex names
namedEL<-function(ln){
	fout<-vector()
  for(i in 1:length(ln)){
  	if(is.network(ln[[i]])){
      # take the edge list from the i'th network in list
  		out<-as.edgelist(ln[[i]])
      # replace the vertex ids by their names, then combine the edges together into one list
      # rbind is kinda slow :|
  		fout<-rbind(fout,snel(out))
  		}
  	}
  unique(fout)
}

## match named edgelist to id'd edgelist
# nel: named edgelist (the vertices are names)
# nam: vertex names list
# returns: turns the names into the edgelist into the number (id) in vertex list
mvnum<-function(nel,nam){
  cbind(match(nel[,1],nam),match(nel[,2],nam))
}

#### Builds edge index map 
# net: base network for the nD object. 
# e: edgelist with vertices numbered according to the ones in net
# returns the edge id's of the edges given by e, in net
mapen<-function(net,e){
  apply(e,1,function(y){get.edgeIDs(net,v=y[1],alter=y[2])})
}

###############
#### Builds a networkDynamic object from edgelist + spell and network object
###############
# net: network object
# x: edge list with spells, in the format:
#    edgeID start end tail head left.censored right.censored
networkDynamicInternal<-function(net,x){

  # x: integer
  # y: table of frequencies
  # returns the table indices that match up with x
  dupf<-function(x,y,v=">"){as.numeric(names(y)[do.call(v,list(y,x))])}

  # set the end points to Inf if the spells are censored
  # make sure the censoring columns are logical (not numeric)
  # note that censoring will override end times
  if("right.censored"%in%colnames(x)){x$end[as.logical(x$right.censored)]<- Inf}
  if("left.censored"%in%colnames(x)){x$start[as.logical(x$left.censored)]<- -Inf}
  
  # activate the edges in the spell list (x[,1] is the edge id's)
  net<-activate.edges(net,onset=x$start,terminus=x$end,e=x[,1])
  
  ###########
  ## Step 2: add multispells
  ###########
  # why is this necessary??
  
  # builds frequency table of edges being used, index by edge id's
  tab<-table(x[,1])
  ### 2.1 add all 2< spells (what?)
  # if any of the edges are used in multiple spells
  if(any(tab>1)){
    for(i in 2:max(tab)){
      # get the edges that appear i times
    	index<-dupf(i,tab,"==")
      # if there are edges that appear i times
    	if(sum(index)!=0){
        # take the spells that are in those edges
      	temp<-x[x[,1]%in%index,]
        # sort them
      	temp<-temp[order(temp[,1]),]
      	#cat("i",i,"\n")
      	for(j in 1:i){
        	t<-temp[seq(j,nrow(temp),i),]
        	net<-activate.edges(net,onset=t[,2],terminus=t[,3],e=t[,1])	
      	}
      	#cat(" j",j,"\n")
    	}
    }
  }
  net
}

# if x is an nD object, return x
# otherwise, do something weird to it??
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
get.vertex.id = function (net, vertex.name) {
  if (!is.network(net)) stop("Error: argument is not a network object")
  temp = which(network.vertex.names(net) == vertex.name)
  if (length(temp) == 0) stop("Error: vertex name not found")
  if (length(temp) > 1) warning("Warning: vertex names are not unique!")
  return(temp[1])
}
