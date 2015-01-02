/*
 *  File networkDynamic/src/edgetree_R.h
 *  Part of the statnet package, http://statnetproject.org
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) in
 *    http://statnetproject.org/attribution
 *
 *  Copyright 2015 the statnet development team
 */
#ifndef EDGETREE_R_H
#define EDGETREE_R_H

#include <Rinternals.h>
#include "edgetree.h"

/* Initialization and destruction. */
SEXP NetworkInitialize_R(SEXP tails, SEXP heads, SEXP nedges,
			  SEXP nnodes, SEXP directed_flag, SEXP bipartite,
			  SEXP lasttoggle_flag, SEXP time, SEXP lasttoggle);
SEXP NetworkCopy_R(SEXP nw);

/* Accessors. */
//Edge EdgetreeSearch (Vertex a, Vertex b, TreeNode *edges);
//Edge EdgetreeSuccessor (TreeNode *edges, Edge x);
//Edge EdgetreePredecessor (TreeNode *edges, Edge x);
//Edge EdgetreeMinimum (TreeNode *edges, Edge x);
//Edge EdgetreeMaximum (TreeNode *edges, Edge x);
SEXP EdgetreeIsDirected_R(SEXP nw);
SEXP GetNeighborhood_R(SEXP nw, SEXP vertex, SEXP type);

/* Modifiers. */

/* *** don't forget,  tails -> heads, so all the functions below using
   heads & tails, now list tails before heads */

//int ToggleEdge (Vertex tail, Vertex head, Network *nwp);
//int ToggleEdgeWithTimestamp (Vertex tail, Vertex head, Network *nwp);
SEXP AddDelEdgeToTrees_R(SEXP tail, SEXP head, SEXP nw, SEXP add);
//void AddHalfedgeToTree (Vertex a, Vertex b, TreeNode *edges, Edge next_edge);
//void UpdateNextedge (TreeNode *edges, Edge *nextedge, Network *nwp);
//int DeleteHalfedgeFromTree(Vertex a, Vertex b, TreeNode *edges,
//		     Edge *next_edge);

/* Duration functions. */
//int ElapsedTime(Vertex tail, Vertex head, Network *nwp);
//void TouchEdge(Vertex tail, Vertex head, Network *nwp);

/* Utility functions. */
SEXP FindithEdge_R (SEXP i, SEXP nw);
//int GetRandEdge(Vertex *tail, Vertex *head, Network *nwp);
//void printedge(Edge e, TreeNode *edges);
//void InOrderTreeWalk(TreeNode *edges, Edge x);
SEXP NetworkEdgeList_R(SEXP nw);
//void ShuffleEdges(Vertex *tails, Vertex *heads, Edge nedges);

/* Others... */
//Edge DesignMissing (Vertex a, Vertex b, Network *mnwp);
//Edge EdgeTree2EdgeList(Vertex *tails, Vertex *heads, Network *nwp, Edge nmax);

#endif
