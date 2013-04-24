######################################################################
#
# extract.R
#
# Written by Carter T. Butts <buttsc@uci.edu>.
#
# Last Modified 04/17/09
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/networkDynamic package
#
# This file contains various routines for taking temporal
# extracts/cross-sections of network class objects with dynamic
# extensions.
#
# Contents:
#
# "%t%.network"
# network.extract
# network.dynamic.check
#
######################################################################

#Operator form for network.extract
"%t%"<-function(x,at){
  network.extract(x=x,at=at)
}


#Function to take a temporal extract/cross-section of a dynamically extended
#network object.  Not very long-term safe....
#retain.all.verticies added if you want to not remove inactive verticies so networks stay the same size for comparison
network.extract<-function(x,onset=NULL,terminus=NULL,length=NULL, at=NULL,
                               rule=c("any","all"),active.default=TRUE,retain.all.vertices=FALSE){
  # determine which nodes/edges are active
  # nodes activity is straight forward
  # edge activity depends on the activity of the edge, but
  # also the activity of it's in/out (tail/head) nodes
  activeV<-is.active(x=x,onset=onset,terminus=terminus,length=length,at=at,
                     e=NULL,v=seq_len(x%n%"n"), rule=rule, active.default=active.default)
  activeE=logical(0)
  if(length(x$mel)){
    activeE<-is.active(x=x,onset=onset,terminus=terminus,length=length,at=at,
                       e=seq_along(x$mel),v=NULL, rule=rule, active.default=active.default)
    inV = sapply(x$mel, "[[", "inl")    # in nodes of edges
    outV = sapply(x$mel, "[[", "outl")  # out nodes of edges
    activeTH = sapply(seq_along(x$mel), function(x){activeV[inV[x]] && activeV[outV[x]]})  # are head and tail active?
  }
  if(retain.all.vertices)
    newVid = seq(x%n%"n")    # node id mapping
  else
    newVid = cumsum(activeV)
  
  # Create network
  n<-ifelse(retain.all.vertices, x%n%"n", sum(activeV))
  if(n==0)
    return(list())
  net<-network.initialize(n)
  # Set network-level attributes
  net$gal<-as.list(x$gal)
  net%n%"n"<-n
  net%n%"mnext"<-1
  if(net%n%"bipartite">0)
    net%n%"bipartite"<-newVid[net%n%"bipartite"]
  # Set vertex-level attributes
  if(n>0)
    net$val<-as.list(x$val[activeV])
  # Add edges
  if(length(activeV)){
    activeETH = activeE & activeTH
    if(any(activeETH)){
      tail<-as.list(lapply(x$mel[activeETH],function(z){newVid[z$outl]}))
      head<-as.list(lapply(x$mel[activeETH],function(z){newVid[z$inl]}))
      atl<-as.list(lapply(x$mel[activeETH],"[[","atl"))
      nam<-as.list(lapply(x$mel[activeETH],function(z){names(z$atl)}))
      add.edges(net,tail=tail,head=head,names.eval=nam,vals.eval=atl)
    }
  }
  net
}





#Function to check dynamic consistency of network objects.  Not terribly safe,
#long-term.
network.dynamic.check<-function(x,verbose=TRUE){
  if(!is.network(x))
    stop("network.dynamic.check requires an object of class network.\n")
  #Check spell matrix
  chkspellmat<-function(z){
    if(is.null(z))
      TRUE
    else{
      if(length(dim(z))!=2)
        FALSE
      else{
        if(dim(z)[2]!=2)
          FALSE
        else{
          if(NROW(z)==1){ #Single spell - either equal, ordered or Inf,Inf
            if(all(z==Inf)||(z[1,2]>=z[1,1]))
              TRUE
            else
              FALSE
          }else{       #Multiple spells - equal, ordered, non-overlapping
            if(all(z[,2]>=z[,1])&&all(z[-1,1]-z[-NROW(z),2]>=0))
              TRUE
            else
              FALSE
          }
        }
      }
    }
  }
  #Check to ensure that vertex activity matrices are legit
  vertok<-sapply(x$val,function(y){chkspellmat(y$active)})
  if(verbose&&any(!vertok))
    cat("Problems detected with vertex activity matrices.\n")
  problem.vertices<-which(!vertok)
  #Check to ensure that edge activity matrices are OK
  edgeok<-rep(F,length(x$mel))
  for(i in seq_along(x$mel)){
    if(is.null(x$mel[[i]]))
      edgeok[i]<-TRUE
    else{
      y<-x$mel[[i]]
      #Verify that the spell matrix is OK, if it exists, to continue
      if(chkspellmat(y$atl$active)){
        ep<-c(y$outl,y$inl)
        if(is.null(y$atl$active)){  #Edge always active
          act<-matrix(c(-Inf,Inf),nc=2)
        }else
          act<-y$atl$active
        #Verify endpoint activity
        flag<-TRUE
        for(j in seq_len(nrow(act)))         #Check each spell...
          if(any(act[j,]!=c(Inf,Inf)))        #Not a placeholder spell...
            for(k in seq_along(ep))      #...against each endpoint
              if(flag){
                flag<-is.active(x=x,onset=act[j,1],terminus=act[j,2],v=ep[k], rule="all")
              }
        edgeok[i]<-flag
      }
    }
  }
  if(verbose&&any(!edgeok))
    cat("Problems detected with edge activity matrices.\n")
  problem.edges<-which(!edgeok)
  #Return the results
  list(problem.vertexIDs=problem.vertices,problem.edgeIDs=problem.edges)
}
