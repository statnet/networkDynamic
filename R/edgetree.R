#  File networkDynamic/R/edgetree.R
#  Part of the statnet package, http://statnetproject.org
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) in
#    http://statnetproject.org/attribution
#
#  Copyright 2012 the statnet development team
######################################################################
# This file contains various routines for accessing edgetree style network objects.
#
# Contents:
#
######################################################################

et.initialize <- function(tails, heads, nedges, nnodes, directed_flag=0L, bipartite=0L,
                        lasttoggle_flag=0L, time=0L, lasttoggle=0L){

  .Call(NetworkInitialize_R, tails, heads, 
        as.integer(nedges), 
        as.integer(nnodes),
        as.integer(directed_flag), 
        as.integer(bipartite),
        as.integer(lasttoggle_flag), 
        as.integer(time), 
        if (is.integer(lasttoggle)) lasttoggle else as.integer(lasttoggle))
}

et.copy <- function(nw) .Call(NetworkCopy_R,nw)

et.print <- function(nw) .Call(NetworkEdgeList_R,nw)

et.add.edges <- function(tail, head, nw) invisible(.Call(AddDelEdgeToTrees_R, tail, head, nw, TRUE))
et.delete.edges <- function(tail, head, nw) invisible(.Call(AddDelEdgeToTrees_R, tail, head, nw, FALSE))

et.find.ith.edge <- function(i, nw) .Call(FindithEdge_R, i, nw)


et.info <- function(nw) .Call(EdgetreeInfo_R,nw)
et.network.size <- function(nw) .Call(EdgetreeInfo_R,nw)$nnodes
et.is.directed <- function(nw) .Call(EdgetreeInfo_R, nw)$directed_flag==1

# type: 1=out, 2=in, 3=combined
et.get.neighborhood <- function(nw, x, type=1L){
   unique(sort(.Call(GetNeighborhood_R, nw, x, as.integer(type))))
}

et.to.edgelist <- function(nw) .Call(EdgeTree2EdgeList_R,nw,TRUE)
et.get.eid <- function(nw,tail,head) .Call(GetEid_R,nw,as.integer(tail),as.integer(head))
