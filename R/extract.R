#  File networkDynamic/R/extract.R
#  Part of the statnet package, http://statnetproject.org
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) in
#    http://statnetproject.org/attribution
#
#  Copyright 2012 the statnet development team
######################################################################
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
    nullE <- sapply(x$mel, is.null)
    inV = sapply(x$mel[!nullE], "[[", "inl")  # in nodes of edges
    outV = sapply(x$mel[!nullE], "[[", "outl")  # out nodes of edges
    activeTH = sapply(1:length(inV), function(x){activeV[inV[x]] && activeV[outV[x]]})  # are head and tail active?
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
  if(length(activeE)){
    activeETH = activeE & activeTH
    if(any(activeETH)){
      tail<-as.list(lapply(x$mel[!nullE][activeETH],function(z){newVid[z$outl]}))
      head<-as.list(lapply(x$mel[!nullE][activeETH],function(z){newVid[z$inl]}))
      atl<-as.list(lapply(x$mel[!nullE][activeETH],"[[","atl"))
      nam<-as.list(lapply(x$mel[!nullE][activeETH],function(z){names(z$atl)}))
      add.edges(net,tail=tail,head=head,names.eval=nam,vals.eval=atl)
    }
  }
  set.nD.class(net)
}



#Function to check dynamic consistency of network objects.  Not terribly safe,
#long-term.
network.dynamic.check<-function(x,verbose=TRUE, complete=TRUE){
  if(!is.network(x))
    stop("network.dynamic.check requires an object of class network.\n")
  pass.complete=TRUE
  if (complete) {
    #Check spell matrix
    chkspellmat<-function(z){
      goodDims=TRUE
      goodSpells=TRUE
      if(!is.null(z)) {
        if(length(dim(z))!=2) {
          goodDims=FALSE
        } else {
          if(dim(z)[2]!=2) {
            goodDims=FALSE
          } else {
            if(NROW(z)==1){ #Single spell - either equal, ordered or Inf,Inf
              if(all(z==Inf)||(z[1,2]>=z[1,1]))
                goodSpells=TRUE
              else
                goodSpells=FALSE
            }else{       #Multiple spells - equal, ordered, non-overlapping
              if(all(z[,2]>=z[,1])&&all(z[-1,1]-z[-NROW(z),2]>=0))
                goodSpells=TRUE
              else
                goodSpells=FALSE
            }
          }
        }
      }
      c(goodDims, goodSpells)
    }
    #Check to ensure that vertex activity matrices are legit
    vertok<-sapply(x$val,function(y){chkspellmat(y$active)})
    if(verbose && any(!vertok[1,])) {
      cat("The dimensionality of the spell matrices is incorrect for vertex/vertices ")
      cat(which(!vertok[1,]))
      cat(".\n")
    }
    if(verbose && any(!vertok[2,])) {
      cat("The ordering of the spell matrices is incorrect for vertex/vertices ")
      cat(which(!vertok[2,]))
      cat(".\n")
    }
    vertok = apply(vertok, 2, function(x){x[1] & x[2]})
    if(any(!vertok)) pass.complete=FALSE
    #Check to ensure that edge activity matrices are OK
    active <- lapply(lapply(x$mel, "[[", "atl"), "[[", "active")
    edgeok <- sapply(active, chkspellmat)
    if(verbose && any(!edgeok[1,])) {
      cat("The dimensionality of the spell matrices is incorrect for edge(s) ")
      cat(which(!edgeok[1,]))
      cat(".\n")
    }
    if(verbose && any(!edgeok[2,])) {
      cat("The ordering of the spell matrices is incorrect for edge(s) ")
      cat(which(!edgeok[2,]))
      cat(".\n")
    }
    edgeok = apply(edgeok, 2, function(x){x[1] & x[2]})
    if(any(!edgeok)) pass.complete=FALSE
  }

  if(pass.complete) {  
    dyadok<-rep(F,length(x$mel))
    for(i in seq_along(x$mel)) {
      if(is.null(x$mel[[i]])) {
        dyadok[i]<-TRUE
      } else {
        y<-x$mel[[i]]
        ep<-c(y$outl,y$inl)
        act<-y$atl$active
        if (is.null(act)) {
          dyadok[i] <- TRUE
        } else {
          #Verify endpoint activity
          flag<-TRUE
          for(j in seq_len(nrow(act)))          #Check each spell...
            if(any(act[j,]!=c(Inf,Inf)))        #Not a placeholder spell...
              for(k in seq_along(ep))           #...against each endpoint
                if(flag) {
                  active.try = try(flag <- is.active(x=x,onset=act[j,1],terminus=act[j,2],v=ep[k],
                                   rule="all"), silent=T)
                  if(class(active.try)=="try-error") {
                    flag <- FALSE
                    warn.text="This check encountered an error in the is.active function."
                    if(!complete) warn.text=paste(warn.text, "Re-run check with 'complete=T'")
                    warning(warn.text)
                  } else {
                    flag <- active.try
                  }
                }
          dyadok[i]<-flag
        }
      }
    }
    if(verbose && any(!dyadok)){
      cat("Edges were found active where the endpoints where not in edge(s) ")
      cat(which(!dyadok))
      cat(".\n")
    }
  }

  #Return the results
  if(complete && pass.complete) {
    list(vertex.checks=vertok, edge.checks=edgeok, dyad.checks=dyadok)
  } else if (complete) {
    list(vertex.checks=vertok, edge.checks=edgeok)
  } else {
    list(dyad.checks=dyadok)
  }
}

