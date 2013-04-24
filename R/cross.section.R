######################################################################
#
# cross.section.R
#
# Written by Carter T. Butts <buttsc@uci.edu>.
#
# Last Modified 04/17/09
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/networkDynamic package
#
# This file contains various routines for taking temporal cross-sections of
# network class objects with dynamic extensions.
#
# Contents:
#
# "%t%.network"
# network.crosssection
# network.dynamic.check
#
######################################################################

#Operator form for network.crosssection
"%t%"<-function(x,onset){
  network.crosssection(x=x,onset=onset)
}


#Function to take a temporal cross-section of a dynamically extended
#network object.  Not very long-term safe....
network.crosssection<-function(x,onset,terminus=NULL,rule=c("any","all"),active.default=TRUE){
  valv<-is.active(x=x,onset=onset,terminus=terminus,e=NULL,v=1:network.size(x), rule=rule, active.default=active.default)
  vale<-is.active(x=x,onset=onset,terminus=terminus,e=1:length(x$mel),v=NULL, rule=rule, active.default=active.default)
  #Create network
  n<-sum(valv)
  if(n==0)
    return(list())
  net<-network.initialize(n)
  #Set network-level attributes
  net$gal<-as.list(x$gal)
  net%n%"n"<-n
  net%n%"mnext"<-1
  if(net%n%"bipartite">0)
    net%n%"bipartite"<-sum(is.active(x=x,onset=onset,terminus=terminus,e=NULL, v=1:(x%n%"bipartite"),rule=rule, active.default=active.default))
  #Set vertex-level attributes
  if(n>0){
    net$val<-as.list(x$val[valv])
  }
  #Add edges
  if(sum(vale)>0){
    newvid<-cumsum(valv)
    tail<-as.list(lapply(x$mel[vale],function(z){newvid[z$outl]}))
    head<-as.list(lapply(x$mel[vale],function(z){newvid[z$inl]}))
    atl<-as.list(lapply(x$mel[vale],"[[","atl"))
    nam<-as.list(lapply(x$mel[vale],function(z){names(z$atl)}))
    add.edges(net,tail=tail,head=head,names.eval=nam,vals.eval=atl)
  }
  #Return the results
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
          if(NROW(z)==1){        #Single spell - either ordered or Inf,Inf
            if(all(z==Inf)||(z[1,2]>z[1,1]))
              TRUE
            else
              FALSE
          }else{                 #Multiple spells - ordered, non-overlapping
            if(all(z[,2]>z[,1])&&all(z[-1,1]-z[-NROW(z),2]>=0))
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
  for(i in 1:length(x$mel)){
    if(is.null(x$mel[[i]]))
      edgeok[i]<-TRUE
    else{
      y<-x$mel[[i]]
      #Verify that the spell matrix is OK, if it exists, to continue
      if(chkspellmat(y$active)){
        ep<-c(y$outl,y$inl)
        if(is.null(y$atl$active)){  #Edge always active
          act<-matrix(c(-Inf,Inf),nc=2)
        }else
          act<-y$atl$active
        #Verify endpoint activity
        flag<-TRUE
        for(j in 1:NROW(act))         #Check each spell...
          for(k in 1:length(ep))      #...against each endpoint
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
