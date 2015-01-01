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

et.initialize <- function(tails, heads, nedges, nnodes, directed_flag, bipartite,
                        lasttoggle_flag, time, lasttoggle){

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
