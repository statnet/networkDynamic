######################################################################
#
# access.R
#
# Written by Carter T. Butts <buttsc@uci.edu>.
#
# Last Modified 04/17/09
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/networkDynamic package
#
# This file contains various routines for accessing network class objects with
# dynamic extensions.
#
# Contents:
#
#   activate.edges
#   activate.vertices
#   deactivate.edges
#   deactivate.vertices
#   get.edgeIDs.active
#   get.edges.active
#   get.neighborhood.active
#   is.active
#   is.adjacent.active
#   network.dyadcount.active
#   network.edgecount.active
#   network.naedgecount.active
#   network.size.active
#
######################################################################

#Function to activate the selected edges at the appropriate time.  If already
#active, activation has no effect; otherwise, it inserts an onset time at
#the appropriate mark.  Edges without an "active" attribute are given one.
activate.edges<-function(x, onset=-Inf, terminus=NULL, e=1:length(x$mel)){
  e<-e[!sapply(x$mel[e],is.null)]  #Filter out non-edges
  if(length(e)==0)
    return()
  #Get existing activity attributes
  active<-lapply(lapply(x$mel[e],"[[","atl"),"[[","active")
  #print(active)
  #Update as needed
  onset<-rep(onset,length=length(e))
  if(is.null(terminus)){           #Only onset given - trigger activation
    for(i in 1:length(active)){
      if(is.null(active[[i]])){         #If not present, add it
        active[[i]]<-matrix(c(onset[i],Inf),nc=2)  #By default, end at Inf
      }else{
        mat<-active[[i]]
        if(onset[i]<mat[1,1])
          active[[i]][1,1]<-onset[i]
        else if(onset[i]==mat[NROW(mat),2])
          active[[i]][NROW(mat),2]<-Inf
        else if(onset[i]>mat[NROW(mat),2])
          active[[i]]<-rbind(active[[i]],c(onset[i],Inf))
        else{
          afton<-max(which(onset[i]>=mat[,1]))
          befterm<-min(which(onset[i]<mat[,2]))
          if(afton!=befterm){           # Requires adding/extending a new spell
            if(mat[afton,2]==onset[i]){          #Extend/merge spells
              mat[afton,2]<-mat[afton+1,2]
              active[[i]]<-mat[-(afton+1),,drop=FALSE]
            }else{                              #Extend spell
              active[[i]][befterm,1]<-onset[1]
            }
          }
        }
      }
    }
  }else{                          #Activate in onset-terminus interval
    terminus<-rep(terminus,length=length(e))
    for(i in 1:length(active)){
      if(is.null(active[[i]])||all(active[[i]]==Inf)){ #If not present, add it
        active[[i]]<-matrix(c(onset[i],terminus[i]),nc=2)
      }else{
        mat<-active[[i]]
        if(terminus[i]<mat[1,1]){              #Add before current spells
          active[[i]]<-rbind(c(onset[i],terminus[i]),mat)
        }else if(terminus[i]==mat[1,1]){       #Prefix to first spell
          active[[i]][1,1]<-onset[i]
        }else if(onset[i]==mat[NROW(mat),2]){  #Suffix to last spell
          active[[i]][NROW(mat),2]<-terminus[i]
        }else if(onset[i]>mat[NROW(mat),2]){   #Add after current spells
          active[[i]]<-rbind(active[[i]],c(onset[i],terminus[i]))
        }else if(terminus[i]>=mat[NROW(mat),2]){  #Terminus after current spells
          if(onset[i]<mat[1,1]){                    #Replace all
            active[[i]]<-matrix(c(onset[i],terminus[i]),nc=2)
          }else{                                    #Replace some
            oafton<-max(which(onset[i]>=mat[,1]))
            obefterm<-min(which(onset[i]<mat[,2]))
            if(oafton==obefterm)
              active[[i]][oafton,2]<-terminus[i]
            if(oafton+1<=NROW(mat))
              active[[i]]<-active[[i]][-((oafton+1):NROW(mat)),,drop=FALSE]
          }
        }else if(onset[i]<mat[1,1]){              #Onset before current spells
          tafton<-max(which(terminus[i]>=mat[,1]))
          tbefterm<-min(which(terminus[i]<mat[,2]))
          if(tafton==tbefterm)
            active[[i]][tafton,1]<-onset[i]
          if(tafton>1)
            active[[i]]<-active[[i]][-(1:tafton),,drop=FALSE]
        }else{                            #Must be within current spells
          oafton<-max(which(onset[i]>=mat[,1]))
          obefterm<-min(which(onset[i]<mat[,2]))
          tafton<-max(which(terminus[i]>=mat[,1]))
          tbefterm<-min(which(terminus[i]<mat[,2]))
          if(oafton==obefterm){   #Onset is within oafton spell
            if(tafton==tbefterm){   #Terminus is within tafton spell
              if(tafton>oafton){      #Merge multiple spells
                mat[oafton,2]<-mat[tafton,2]
                active[[i]]<-mat[-((oafton+1):tafton),,drop=FALSE] 
              }
            }else{                  #Terminus is between tafton and tafton+1
              if(terminus[i]==mat[tafton,2]){ #Merge through tafton+1/tbefterm
                mat[oafton,2]<-mat[tbefterm,2]
                active[[i]]<-mat[-((oafton+1):tbefton),,drop=FALSE] 
              }else{
                if(tafton==oafton){      #Extend oafton
                  active[[i]][oafton,2]<-terminus[i]
                }else{                   #Merge and extend
                  mat[oafton,2]<-terminus[i]
                  active[[i]]<-mat[-((oafton+1):tafton),,drop=FALSE]
                }
              }
            }
          }else{                  #Onset is between oafton and oafton+1
            if(tafton==tbefterm){   #Terminus is within tafton spell
              if(onset[i]==mat[oafton,2]){  #Merge oafton through tafton
                mat[oafton,2]<-mat[tafton,2]
                active[[i]]<-mat[-((oafton+1):tafton),,drop=FALSE]
              }else{                        #Extend tafton backwards
                if(obefterm==tafton){
                  active[[i]][tafton,1]<-onset[i]
                }else{
                  mat[tafton,1]<-onset[i]
                  active[[i]]<-mat[-(obefterm:(tafton-1)),,drop=FALSE]
                }
              }
            }else{                  #Terminus is between tafton and tafton+1
              active[[i]]<-rbind(mat[1:oafton,,drop=FALSE], c(onset[i],terminus[i]), mat[tbefterm:NROW(mat),,drop=FALSE])
            }
          }
        }
      }
    }
  }
  #print(active)
  #Write the new attributes into the network object
  set.edge.attribute(x=x,attrname="active",value=active,e=e)
}


#Function to activate the selected vertices at the appropriate time.  If already
#active, activation has no effect; otherwise, it inserts an onset time at
#the appropriate mark.  Vertices without an "active" attribute are given one.
activate.vertices<-function(x, onset=-Inf, terminus=NULL, v=1:network.size(x)){
  #Get existing activity attributes
  active<-lapply(x$val[v],"[[","active")
  #print(active)
  #Update as needed
  onset<-rep(onset,length=length(v))
  if(is.null(terminus)){           #Only onset given - trigger activation
    for(i in 1:length(active)){
      if(is.null(active[[i]])){         #If not present, add it
        active[[i]]<-matrix(c(onset[i],Inf),nc=2)  #By default, end at Inf
      }else{
        mat<-active[[i]]
        if(onset[i]<mat[1,1])
          active[[i]][1,1]<-onset[i]
        else if(onset[i]==mat[NROW(mat),2])
          active[[i]][NROW(mat),2]<-Inf
        else if(onset[i]>mat[NROW(mat),2])
          active[[i]]<-rbind(active[[i]],c(onset[i],Inf))
        else{
          afton<-max(which(onset[i]>=mat[,1]))
          befterm<-min(which(onset[i]<mat[,2]))
          if(afton!=befterm){           # Requires adding/extending a new spell
            if(mat[afton,2]==onset[i]){          #Extend/merge spells
              mat[afton,2]<-mat[afton+1,2]
              active[[i]]<-mat[-(afton+1),,drop=FALSE]
            }else{                              #Extend spell
              active[[i]][befterm,1]<-onset[1]
            }
          }
        }
      }
    }
  }else{                          #Activate in onset-terminus interval
    terminus<-rep(terminus,length=length(v))
    for(i in 1:length(active)){
      if(is.null(active[[i]])||all(active[[i]]==Inf)){ #If not present, add it
        active[[i]]<-matrix(c(onset[i],terminus[i]),nc=2)
      }else{
        mat<-active[[i]]
        if(terminus[i]<mat[1,1]){              #Add before current spells
          active[[i]]<-rbind(c(onset[i],terminus[i]),mat)
        }else if(terminus[i]==mat[1,1]){       #Prefix to first spell
          active[[i]][1,1]<-onset[i]
        }else if(onset[i]==mat[NROW(mat),2]){  #Suffix to last spell
          active[[i]][NROW(mat),2]<-terminus[i]
        }else if(onset[i]>mat[NROW(mat),2]){   #Add after current spells
          active[[i]]<-rbind(active[[i]],c(onset[i],terminus[i]))
        }else if(terminus[i]>=mat[NROW(mat),2]){  #Terminus after current spells
          if(onset[i]<mat[1,1]){                    #Replace all
            active[[i]]<-matrix(c(onset[i],terminus[i]),nc=2)
          }else{                                    #Replace some
            oafton<-max(which(onset[i]>=mat[,1]))
            obefterm<-min(which(onset[i]<mat[,2]))
            if(oafton==obefterm)
              active[[i]][oafton,2]<-terminus[i]
            if(oafton+1<=NROW(mat))
              active[[i]]<-active[[i]][-((oafton+1):NROW(mat)),,drop=FALSE]
          }
        }else if(onset[i]<mat[1,1]){              #Onset before current spells
          tafton<-max(which(terminus[i]>=mat[,1]))
          tbefterm<-min(which(terminus[i]<mat[,2]))
          if(tafton==tbefterm)
            active[[i]][tafton,1]<-onset[i]
          if(tafton>1)
            active[[i]]<-active[[i]][-(1:tafton),,drop=FALSE]
        }else{                            #Must be within current spells
          oafton<-max(which(onset[i]>=mat[,1]))
          obefterm<-min(which(onset[i]<mat[,2]))
          tafton<-max(which(terminus[i]>=mat[,1]))
          tbefterm<-min(which(terminus[i]<mat[,2]))
          if(oafton==obefterm){   #Onset is within oafton spell
            if(tafton==tbefterm){   #Terminus is within tafton spell
              if(tafton>oafton){      #Merge multiple spells
                mat[oafton,2]<-mat[tafton,2]
                active[[i]]<-mat[-((oafton+1):tafton),,drop=FALSE] 
              }
            }else{                  #Terminus is between tafton and tafton+1
              if(terminus[i]==mat[tafton,2]){ #Merge through tafton+1/tbefterm
                mat[oafton,2]<-mat[tbefterm,2]
                active[[i]]<-mat[-((oafton+1):tbefton),,drop=FALSE] 
              }else{
                if(tafton==oafton){      #Extend oafton
                  active[[i]][oafton,2]<-terminus[i]
                }else{                   #Merge and extend
                  mat[oafton,2]<-terminus[i]
                  active[[i]]<-mat[-((oafton+1):tafton),,drop=FALSE]
                }
              }
            }
          }else{                  #Onset is between oafton and oafton+1
            if(tafton==tbefterm){   #Terminus is within tafton spell
              if(onset[i]==mat[oafton,2]){  #Merge oafton through tafton
                mat[oafton,2]<-mat[tafton,2]
                active[[i]]<-mat[-((oafton+1):tafton),,drop=FALSE]
              }else{                        #Extend tafton backwards
                if(obefterm==tafton){
                  active[[i]][tafton,1]<-onset[i]
                }else{
                  mat[tafton,1]<-onset[i]
                  active[[i]]<-mat[-(obefterm:(tafton-1)),,drop=FALSE]
                }
              }
            }else{                  #Terminus is between tafton and tafton+1
              active[[i]]<-rbind(mat[1:oafton,,drop=FALSE], c(onset[i],terminus[i]), mat[tbefterm:NROW(mat),,drop=FALSE])
            }
          }
        }
      }
    }
  }
  #print(active)
  #Write the new attributes into the network object
  set.vertex.attribute(x=x,attrname="active",value=active,v=v)
}


#Function to deactivate the selected edges at the appropriate time.  If already
#inactive, activation has no effect; otherwise, it inserts a termination time at
#the appropriate mark.  Edges without an "active" attribute are given one.
deactivate.edges<-function(x, onset=Inf, terminus=NULL, e=1:length(x$mel)){
  e<-e[!sapply(x$mel[e],is.null)]  #Filter out non-edges
  if(length(e)==0)
    return()
  #Get existing activity attributes
  active<-lapply(lapply(x$mel[e],"[[","atl"),"[[","active")
  #print(active)
  #Update as needed
  onset<-rep(onset,length=length(e))
  if(is.null(terminus)){           #Only onset given - trigger deactivation
    for(i in 1:length(active)){
      if(is.null(active[[i]])){         #If not present, add it
        active[[i]]<-matrix(c(-Inf,onset[i]),nc=2)  #By default, start at -Inf
      }else{
        mat<-active[[i]]
        if((onset[i]>=mat[1,1])&&(onset[i]<mat[NROW(mat),2])){
          afton<-max(which(onset[i]>=mat[,1]))
          befterm<-min(which(onset[i]<mat[,2]))
          if(afton==befterm){                # Requires spell removal/truncation
            if(mat[afton,1]==onset[i]){             #Delete the spell
              if(NROW(mat)==1)                         #If none left, cope
                active[[i]]<-matrix(c(Inf,Inf),nc=2)
              else
                active[[i]]<-mat[-afton,,drop=FALSE]
            }else{                                 #Truncate the spell
              active[[i]][afton,2]<-onset[i]
            }
          }
        }
      }
    }
  }else{                          #Deactivate in onset-terminus interval
    terminus<-rep(terminus,length=length(e))
    for(i in 1:length(active)){
      if(is.null(active[[i]])){         #If not present, add it
        active[[i]]<-rbind(c(-Inf,onset[i]),c(terminus[i],Inf))
      }else if(!all(active[[i]]==Inf)){
        mat<-active[[i]]
        if(onset[i]<=mat[1,1]){   #Onset before first spell
          if(terminus[i]>mat[1,1]){  #Terminus after first spell starts
            if(terminus[i]>=mat[NROW(mat),2])  #Terminus after end
              active[[i]]<-matrix(c(Inf,Inf),nc=2)
            else{
              tafton<-max(which(terminus[i]>=mat[,1]))
              tbefterm<-min(which(terminus[i]<mat[,2]))
              if(tafton==tbefterm){     #Terminus is in tafton
                mat[tafton,1]<-terminus[i]
                if(tafton>1)
                  active[[i]]<-mat[-(1:(tafton-1)),,drop=FALSE]
                else
                  active[[i]]<-mat
              }else{                    #Remove everything before tbefterm
                active[[i]]<-mat[-(1:tafton),,drop=FALSE]
              }
            }
          }
        }else if(onset[i]<mat[NROW(mat),2]){  #Onset b/w start,end of spells
          oafton<-max(which(onset[i]>=mat[,1]))
          obefterm<-min(which(onset[i]<mat[,2]))
          if(terminus[i]>=mat[NROW(mat),2]){  #Terminus at/after end of spells
            if(oafton==obefterm){       #Onset is in oafton
              mat[oafton,2]<-onset[i]
              if(oafton<NROW(mat))
                active[[i]]<-mat[-(obefterm:NROW(mat)),,drop=FALSE]
              else
                active[[i]]<-mat
            }else{                      #Remove everything after oafton
              active[[i]]<-mat[-(obefterm:NROW(mat)),,drop=FALSE]
            }
          }else{                              #Terminus b/w start,end of spells
            tafton<-max(which(terminus[i]>=mat[,1]))
            tbefterm<-min(which(terminus[i]<mat[,2]))
            if(oafton==obefterm){       #Onset is in oafton
              if(tafton==tbefterm){       #Terminus is in tafton
                if(oafton==tafton){         #Split oafton/tafton
                  mat<-rbind(mat[1:oafton,,drop=FALSE], mat[tafton:NROW(mat),,drop=FALSE])
                  mat[oafton,2]<-onset[i]
                  mat[oafton+1,1]<-terminus[i]
                  active[[i]]<-mat
                }else if(oafton+1==tafton){  #Truncate at both ends
                  active[[i]][oafton,2]<-onset[i]
                  active[[i]][tafton,1]<-terminus[i]
                }else{                      #Truncate and delete internal spells
                  mat[oafton,2]<-onset[i]
                  mat[tafton,1]<-terminus[i]
                  active[[i]]<-mat[c(1:oafton,tafton:NROW(mat)),,drop=FALSE]
                }
              }else{                      #Terminus b/w tafton, tbefterm
                if(oafton==tafton){          #Truncate oafton
                  active[[i]][oafton,2]<-onset[i]
                }else{                       #Truncate and remove spells
                  mat[oafton,2]<-onset[i]
                  active[[i]]<-mat[-((oafton+1):tafton),,drop=FALSE]
                }
              }
            }else{                      #Onset b/w oafton, obefterm
              if(tafton==tbefterm){       #Terminus is in tafton
                if(obefterm==tafton){       #Truncate tafton
                  active[[i]][tafton,1]<-terminus[i]
                }else{
                  mat[tafton,1]<-terminus[i]
                  active[[i]]<-mat[c(1:oafton,tafton:NROW(mat)),,drop=FALSE]
                }
              }else{                      #Terminus b/w tafton, tbefterm
                active[[i]]<-mat[c(1:oafton,tbefterm:NROW(mat)),,drop=FALSE]
              }
            }
          }
        }
      }
    }
  }
  #print(active)
  #Write the new attributes into the network object
  set.edge.attribute(x=x,attrname="active",value=active,e=e)
}


#Function to deactivate the selected vertices at the appropriate time.  If 
#already inactive, activation has no effect; otherwise, it inserts a termination
#time at the appropriate mark.  Vertices without an "active" attribute are given
#one.
deactivate.vertices<-function(x, onset=Inf, terminus=NULL, v=1:network.size(x)){
  #Get existing activity attributes
  active<-lapply(x$val[v],"[[","active")
  #print(active)
  #Update as needed
  onset<-rep(onset,length=length(v))
  if(is.null(terminus)){           #Only onset given - trigger deactivation
    for(i in 1:length(active)){
      if(is.null(active[[i]])){         #If not present, add it
        active[[i]]<-matrix(c(-Inf,onset[i]),nc=2)  #By default, start at -Inf
      }else{
        mat<-active[[i]]
        if((onset[i]>=mat[1,1])&&(onset[i]<mat[NROW(mat),2])){
          afton<-max(which(onset[i]>=mat[,1]))
          befterm<-min(which(onset[i]<mat[,2]))
          if(afton==befterm){                # Requires spell removal/truncation
            if(mat[afton,1]==onset[i]){             #Delete the spell
              if(NROW(mat)==1)                         #If none left, cope
                active[[i]]<-matrix(c(Inf,Inf),nc=2)
              else
                active[[i]]<-mat[-afton,,drop=FALSE]
            }else{                                 #Truncate the spell
              active[[i]][afton,2]<-onset[i]
            }
          }
        }
      }
    }
  }else{                          #Deactivate in onset-terminus interval
    terminus<-rep(terminus,length=length(v))
    for(i in 1:length(active)){
      if(is.null(active[[i]])){         #If not present, add it
        active[[i]]<-rbind(c(-Inf,onset[i]),c(terminus[i],Inf))
      }else if(!all(active[[i]]==Inf)){
        mat<-active[[i]]
        if(onset[i]<=mat[1,1]){   #Onset before first spell
          if(terminus[i]>mat[1,1]){  #Terminus after first spell starts
            if(terminus[i]>=mat[NROW(mat),2])  #Terminus after end
              active[[i]]<-matrix(c(Inf,Inf),nc=2)
            else{
              tafton<-max(which(terminus[i]>=mat[,1]))
              tbefterm<-min(which(terminus[i]<mat[,2]))
              if(tafton==tbefterm){     #Terminus is in tafton
                mat[tafton,1]<-terminus[i]
                if(tafton>1)
                  active[[i]]<-mat[-(1:(tafton-1)),,drop=FALSE]
                else
                  active[[i]]<-mat
              }else{                    #Remove everything before tbefterm
                active[[i]]<-mat[-(1:tafton),,drop=FALSE]
              }
            }
          }
        }else if(onset[i]<mat[NROW(mat),2]){  #Onset b/w start,end of spells
          oafton<-max(which(onset[i]>=mat[,1]))
          obefterm<-min(which(onset[i]<mat[,2]))
          if(terminus[i]>=mat[NROW(mat),2]){  #Terminus at/after end of spells
            if(oafton==obefterm){       #Onset is in oafton
              mat[oafton,2]<-onset[i]
              if(oafton<NROW(mat))
                active[[i]]<-mat[-(obefterm:NROW(mat)),,drop=FALSE]
              else
                active[[i]]<-mat
            }else{                      #Remove everything after oafton
              active[[i]]<-mat[-(obefterm:NROW(mat)),,drop=FALSE]
            }
          }else{                              #Terminus b/w start,end of spells
            tafton<-max(which(terminus[i]>=mat[,1]))
            tbefterm<-min(which(terminus[i]<mat[,2]))
            if(oafton==obefterm){       #Onset is in oafton
              if(tafton==tbefterm){       #Terminus is in tafton
                if(oafton==tafton){         #Split oafton/tafton
                  mat<-rbind(mat[1:oafton,,drop=FALSE], mat[tafton:NROW(mat),,drop=FALSE])
                  mat[oafton,2]<-onset[i]
                  mat[oafton+1,1]<-terminus[i]
                  active[[i]]<-mat
                }else if(oafton+1==tafton){  #Truncate at both ends
                  active[[i]][oafton,2]<-onset[i]
                  active[[i]][tafton,1]<-terminus[i]
                }else{                      #Truncate and delete internal spells
                  mat[oafton,2]<-onset[i]
                  mat[tafton,1]<-terminus[i]
                  active[[i]]<-mat[c(1:oafton,tafton:NROW(mat)),,drop=FALSE]
                }
              }else{                      #Terminus b/w tafton, tbefterm
                if(oafton==tafton){          #Truncate oafton
                  active[[i]][oafton,2]<-onset[i]
                }else{                       #Truncate and remove spells
                  mat[oafton,2]<-onset[i]
                  active[[i]]<-mat[-((oafton+1):tafton),,drop=FALSE]
                }
              }
            }else{                      #Onset b/w oafton, obefterm
              if(tafton==tbefterm){       #Terminus is in tafton
                if(obefterm==tafton){       #Truncate tafton
                  active[[i]][tafton,1]<-terminus[i]
                }else{
                  mat[tafton,1]<-terminus[i]
                  active[[i]]<-mat[c(1:oafton,tafton:NROW(mat)),,drop=FALSE]
                }
              }else{                      #Terminus b/w tafton, tbefterm
                active[[i]]<-mat[c(1:oafton,tbefterm:NROW(mat)),,drop=FALSE]
              }
            }
          }
        }
      }
    }
  }
  #print(active)
  #Write the new attributes into the network object
  set.vertex.attribute(x=x,attrname="active",value=active,v=v)
}


#Variant of get.edgeIDs with dynamic query support
get.edgeIDs.active<-function(x,v,onset,terminus=NULL,alter=NULL,neighborhood=c("out", "in", "combined"),rule=c("any","all"),na.omit=TRUE,active.default=TRUE){
  #Initially, just get all relevent edge IDs
  eid<-get.edgeIDs(x=x,v=v,alter=alter,neighborhood=neighborhood, na.omit=na.omit)
  #Now, filter by activity
  eid[is.active(x=x,onset=onset,terminus=terminus,e=eid,v=NULL,rule=rule, active.default=active.default)]
}


#Variant of get.edges with dynamic query support.  (Note: not safe in the long
#run...)
get.edges.active<-function(x,v,onset,terminus=NULL,alter=NULL,neighborhood=c("out", "in", "combined"),rule=c("any","all"),na.omit=TRUE,active.default=TRUE){
  #Initially, get all relevent edge IDs
  eid<-get.edgeIDs(x=x,v=v,alter=alter,neighborhood=neighborhood, na.omit=na.omit)
  #Return associated edges, filtered by activity
  if(length(eid)>0)
    x$mel[[is.active(x=x,onset=onset,terminus=terminus,e=eid,v=NULL,rule=rule, active.default=active.default)]]
  else
    list()
}


#Variant of get.neighborhood with dynamic query support.  Slow, most likely.
get.neighborhood.active<-function(x,v,onset,terminus=NULL,type=c("out", "in", "combined"),rule=c("any","all"),na.omit=TRUE,active.default=TRUE){
  if(!is.directed(x)){
    #Get all edges for the neighborhood in question
    el<-get.edges.active(x=x,v=v,onset=onset,terminus=terminus,alter=NULL, neighborhood="out",rule=rule,na.omit=na.omit,active.default=active.default)
    if(length(el)>0){
      #Assemble the neighborhood
      neigh<-sort(unique(c(sapply(el,"[[","inl"),sapply(el,"[[","outl"))))
      #Loop check
      if(!any(sapply(el,function(z){(v%in%z[["inl"]])&&(v%in%z[["outl"]])})))
        neigh<-neigh[neigh!=v]
    }else
      neigh<-integer(0)
  }else{
    if(match.arg(type)=="out"){
      #Get all edges for the neighborhood in question
      el<-get.edges.active(x=x,v=v,onset=onset,terminus=terminus,alter=NULL, neighborhood="out",rule=rule,na.omit=na.omit,active.default=active.default)
      if(length(el)>0){
        #Assemble the neighborhood
        neigh<-sort(unique(sapply(el,"[[","inl")))
      }else
        neigh<-integer(0)
    }else if(match.arg(type)=="in"){
      #Get all edges for the neighborhood in question
      el<-get.edges.active(x=x,v=v,onset=onset,terminus=terminus,alter=NULL, neighborhood="in",rule=rule,na.omit=na.omit,active.default=active.default)
      if(length(el)>0){
        #Assemble the neighborhood
        neigh<-sort(unique(sapply(el,"[[","outl")))
      }else
        neigh<-integer(0)
    }else{
      #Get all edges for the out-neighborhood
      el<-get.edges.active(x=x,v=v,onset=onset,terminus=terminus,alter=NULL, neighborhood="out",rule=rule,na.omit=na.omit,active.default=active.default)
      if(length(el)>0){
        #Assemble the initial neighborhood
        neigh<-sort(unique(sapply(el,"[[","inl")))
      }else
        neigh<-integer(0)
      #Get all edges for the in-neighborhood
      el<-get.edges.active(x=x,v=v,onset=onset,terminus=terminus,alter=NULL, neighborhood="in",rule=rule,na.omit=na.omit,active.default=active.default)
      if(length(el)>0){
        #Add the new vertices
        neigh<-sort(unique(c(neigh,sapply(el,"[[","outl"))))
      }
    }
  }
  #Return the result
  neigh
}


#Function to assess activity of edges (e) or vertices (v) at a given point
#or in a given interval.  If an interval is specified, then rule=="any" 
#returns TRUE for elements active at any time in the interval.  The rule=="all"
#setting returns TRUE for elements active during the entire interval.  Unless
#given either e or v, the function returns NA.
#
#Note that there are a lot of complications here surrounding Inf values.  If
#an activity spell starts at time Inf, it can never match anything (including
#query onsets of Inf).  If an activity spell starts at finite time and ends
#at Inf, however, it _does_ match an onset/terminus of Inf.  By turns, a 
#spell which begins at time -Inf should match -Inf onset times.  All this is
#very annoying, and makes me wish that I'd just outlawed infinity.  But that's
#how things are.
is.active<-function(x,onset,terminus=NULL,e=NULL,v=NULL,rule=c("any","all"),active.default=TRUE){
  #Preliminaries
  if(!is.network(x))
    stop("Object of class network required.\n")
  if(length(e)*length(v)>0)
    stop("Either edges or vertices must be specified (not both).\n")
  rule<-match.arg(rule)
  #Run the check
  if(length(e)){            #Check edges
    onset<-rep(onset,length=length(e))
    if(!is.null(terminus))
      terminus<-rep(terminus,length=length(e))
    active<-lapply(lapply(x$mel[e],"[[","atl"),"[[","active")
    act<-rep(FALSE,length(active))
    for(i in 1:length(e)){
      if(!is.null(x$mel[[i]])){
        if(is.null(active[[i]]))
          act[i]<-active.default
        else if(((onset[i]==Inf)&&(active[[i]][NROW(active[[i]]),2]==Inf)) || (onset[i]<active[[i]][NROW(active[[i]]),2])){ #Onset before end
          if(is.null(terminus)){   #Point intersection query
            if((active[[i]][1,1]<Inf)&&(onset[i]>=active[[i]][1,1])){
              afton<-which(onset[i]>=active[[i]][,1])
              if(length(afton)>0)
                afton<-max(afton)
              befterm<-which((active[[i]][,2]==Inf)|(onset[i]<active[[i]][,2]))
              if(length(befterm)>0)
                befterm<-min(befterm)
              act[i]<-((length(afton)*length(befterm)>0)&&(afton==befterm))
            }
          }else{                   #Interval query
            if(terminus[i]>active[[i]][1,1]){  #Terminus after beginning
              oafton<-which(onset[i]>=active[[i]][,1])
              if(length(oafton)>0)
                oafton<-max(oafton)
              obefterm<-which((active[[i]][,2]==Inf)|(onset[i]<active[[i]][,2]))
              if(length(obefterm)>0)
                obefterm<-min(obefterm)
              tafton<-which(terminus[i]>=active[[i]][,1])
              if(length(tafton)>0)
                tafton<-max(tafton)
              tbefterm<-which((active[[i]][,2]==Inf)| (terminus[i]<active[[i]][,2]))
              if(length(tbefterm)>0)
                tbefterm<-min(tbefterm)
              if(length(oafton)*length(tafton)*length(obefterm)* length(tbefterm)>0){
                if(rule=="any"){
                  if((oafton!=tafton)||(oafton==obefterm)) #Two conditions
                    act[i]<-TRUE
                }else{
                  if((oafton==tafton)&&(oafton==obefterm)) #One condition
                    act[i]<-TRUE
                }
              }
            }
          }
        }
      }
    }
  }else{
    if(length(v)){          #Check vertices
      onset<-rep(onset,length=length(v))
      if(!is.null(terminus))
        terminus<-rep(terminus,length=length(v))
      active<-lapply(x$val[v],"[[","active")
      act<-rep(FALSE,length(active))
      for(i in 1:length(v)){
        if(is.null(active[[i]]))
          act[i]<-active.default
        else if(((onset[i]==Inf)&&(active[[i]][NROW(active[[i]]),2]==Inf)) || (onset[i]<active[[i]][NROW(active[[i]]),2])){ #Onset before end
          if(is.null(terminus)){   #Point intersection query
            if((active[[i]][1,1]<Inf)&&(onset[i]>=active[[i]][1,1])){
              afton<-which(onset[i]>=active[[i]][,1])
              if(length(afton)>0)
                afton<-max(afton)
              befterm<-which((active[[i]][,2]==Inf)|(onset[i]<active[[i]][,2]))
              if(length(befterm)>0)
                befterm<-min(befterm)
              act[i]<-((length(afton)*length(befterm)>0)&&(afton==befterm))
            }
          }else{                   #Interval query
            if(terminus[i]>active[[i]][1,1]){  #Terminus after beginning
              oafton<-which(onset[i]>=active[[i]][,1])
              if(length(oafton)>0)
                oafton<-max(oafton)
              obefterm<-which((active[[i]][,2]==Inf)|(onset[i]<active[[i]][,2]))
              if(length(obefterm)>0)
                obefterm<-min(obefterm)
              tafton<-which(terminus[i]>=active[[i]][,1])
              if(length(tafton)>0)
                tafton<-max(tafton)
              tbefterm<-which((active[[i]][,2]==Inf)| (terminus[i]<active[[i]][,2]))
              if(length(tbefterm)>0)
                tbefterm<-min(tbefterm)
              if(length(oafton)*length(tafton)*length(obefterm)* length(tbefterm)>0){
                if(rule=="any"){
                  if((oafton!=tafton)||(oafton==obefterm)) #Two conditions
                    act[i]<-TRUE
                }else{
                  if((oafton==tafton)&&(oafton==obefterm)) #One condition
                    act[i]<-TRUE
                }
              }
            }
          }
        }
      }
    }else
      act<-logical(0)
  }
  act
}


#Variant of is.adjacent for networks with dynamic extensions.  Slow, but will
#get the job done.
is.adjacent.active<-function(x,vi,vj,onset,terminus=NULL,rule=c("any","all"),na.omit=FALSE,active.default=TRUE){
  #Initially, get edge IDs from vi to vj
  eid<-get.edgeIDs(x=x,v=vi,alter=vj,neighborhood="out",na.omit=na.omit)
  #Return TRUE iff any active edges exist
  if(length(eid)==0)
    FALSE
  else
    any(is.active(x=x,onset=onset,terminus=terminus,e=eid,v=NULL,rule=rule, active.default=active.default))
}


#Variant network.dyadcount which uses only active vertices.
network.dyadcount.active<-function (x, onset, terminus=NULL, rule=c("any","all"), na.omit = TRUE, active.default=TRUE) 
{
    if (!is.network(x)) 
        stop("network.dyadcount requires an argument of class network.")
    nodes <- network.size(x=x, onset=onset, terminus=terminus, rule=rule, active.default=active.default)
    if (is.directed(x)) {
        dyads <- nodes * (nodes - 1)
    }
    else {
        if (is.bipartite(x)) {
            nactor <- sum(is.active(x=x,onset=onset,terminus=terminus,e=NULL, v=1:get.network.attribute(x, "bipartite"),rule=rule, active.default=active.default))
            nevent <- sum(is.active(x=x,onset=onset,terminus=terminus,e=NULL, v=(get.network.attribute(x, "bipartite")+1):nodes,rule=rule, active.default=active.default))
            dyads <- nactor * nevent
        }
        else {
            dyads <- nodes * (nodes - 1)/2
        }
    }
    if (na.omit) {
        design <- get.network.attribute(x, "design")
        if (!is.null(design)) {
            dyads <- dyads - network.edgecount.active(designonset=onset,terminus=terminus,rule=rule, active.default=active.default)
        }
        else {
            design <- get.network.attribute(x, "mClist.design")
            if (!is.null(design)) {
                dyads <- dyads - design$nedges
            }
        }
    }
    dyads
}


#Variant network.edgecount which counts only active edges.  Not long-run safe.
network.edgecount.active<-function (x, onset, terminus=NULL, rule=c("any","all"), na.omit = TRUE, active.default=TRUE){
  act<-is.active(x=x,onset=onset,terminus=terminus,e=1:(x%n%"mnext"-1), v=NULL,rule=rule, active.default=active.default)
  act<-act[!sapply(x$mel,is.null)]
  if(na.omit)
    sum(act*(1-(x%e%"na")))
  else
    sum(act)
}


#Variant network.naedgecount which counts only active edges.  Not safe.
network.naedgecount.active<-function (x, onset, terminus=NULL, rule=c("any","all"), active.default=TRUE){
  act<-is.active(x=x,onset=onset,terminus=terminus,e=1:(x%n%"mnext"-1), v=NULL,rule=rule, active.default=active.default)
  act<-act[!sapply(x$mel,is.null)]
  sum(act*(x%e%"na"))
}

#Network size which counts only active vertices - don't use for other purposes!
network.size.active<-function(x,onset,terminus=NULL,rule=c("any","all"),active.default=TRUE){
  sum(is.active(x=x,onset=onset,terminus=terminus,e=NULL,v=1:network.size(x), rule=rule,active.default=active.default))
}

