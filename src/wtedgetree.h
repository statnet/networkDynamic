/*
 *  File networkDynamic/src/wtedgetree.h
 *  Part of the statnet package, http://statnetproject.org
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) in
 *    http://statnetproject.org/attribution
 *
 *  Copyright 2012 the statnet development team
 */
#ifndef WTEDGETREE_H
#define WTEDGETREE_H

#include <math.h>
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include "edgetree.h"

/* WtTreeNode is just like TreeNode but with an extra field for a
   weight, or value, that might be associated with the node */
typedef struct WtTreeNodestruct {
  Vertex value;      /*  the vertex at the other end of the edge  */
  Edge parent;   /*  parent of this node in the tree (0 for root) */
  Edge left;     /*  left child (0 if none)  */
  Edge right;    /*  right child (0 if none) */
  double weight;
} WtTreeNode;





/* Network is a structure containing all essential elements
   of a given network; it is a slightly rewritten version of the old Gptr,
   with some changes of awkard things, deletion of unnecessary things, and
   a new name more reflective of what it does!

   Some of the fields in a Network structure are:
   inedges and outedges are arrays of TreeNode that are used to 
     store all of the incoming and outgoing edges, respectively. 
   directed_flag is 1 or 0, depending on whether or not the 
     network is directed. 
   next_inedge and next_outedge are continually updated to give
     the smallest index of an edge object not being used.  
   outdegree[] and indegree[] are continually updated to give
     the appropriate degree values for each vertex.  These should
     point to Vertex-vectors of length nnodes+1.  
   value:  optional value(s) associated with this network 
   Dur_Inf:  See typedef for Dur_Infstruct above
*/
/* WtNetwork is a structure just like Network except it is for a network with 
   weighted (valued) edges.  */
typedef struct WtNetworkstruct {
  WtTreeNode *inedges;
  WtTreeNode *outedges;
  int directed_flag;
  Vertex bipartite;  
  Vertex nnodes;
  Edge nedges;
  Edge next_inedge;
  Edge next_outedge;
  Vertex *indegree;
  Vertex *outdegree;
  double *value;  
  Dur_Inf duration_info;
  Edge maxedges;
} WtNetwork;

/* Initialization and destruction. */
WtNetwork WtNetworkInitialize(Vertex *tails, Vertex *heads, double *weights, Edge nedges,
			      Vertex nnodes, int directed_flag, Vertex bipartite,
			      int lasttoggle_flag, int time, int *lasttoggle);
void WtNetworkDestroy(WtNetwork *nwp);
WtNetwork WtNetworkInitializeD(double *tails, double *heads, double *weights, Edge nedges,
			       Vertex nnodes, int directed_flag, Vertex bipartite,
			      int lasttoggle_flag, int time, int *lasttoggle);

WtNetwork *WtNetworkCopy(WtNetwork *dest, WtNetwork *src);

/* Accessors. */
Edge WtEdgetreeSearch (Vertex a, Vertex b, WtTreeNode *edges);
double WtGetEdge (Vertex tail, Vertex head, WtNetwork *nwp);
Edge WtEdgetreeSuccessor (WtTreeNode *edges, Edge x);
Edge WtEdgetreePredecessor (WtTreeNode *edges, Edge x);
Edge WtEdgetreeMinimum (WtTreeNode *edges, Edge x);
Edge WtEdgetreeMaximum (WtTreeNode *edges, Edge x);

/* Modifiers. */

/* *** don't forget,  tails -> heads, so all the functions below using
   heads & tails, now list tails before heads */


void WtSetEdge (Vertex tail, Vertex head, double weight, WtNetwork *nwp);
void WtSetEdgeWithTimestamp (Vertex tail, Vertex head, double weight, WtNetwork *nwp);
int WtToggleEdge (Vertex tail, Vertex head, double weight, WtNetwork *nwp);
int WtToggleEdgeWithTimestamp (Vertex tail, Vertex head, double weight, WtNetwork *nwp);
int WtAddEdgeToTrees(Vertex tail, Vertex head, double weight, WtNetwork *nwp);
void WtAddHalfedgeToTree (Vertex a, Vertex b, double weight, WtTreeNode *edges, Edge next_edge);
void WtUpdateNextedge (WtTreeNode *edges, Edge *nextedge, WtNetwork *nwp);
int WtDeleteEdgeFromTrees(Vertex tail, Vertex head, WtNetwork *nwp);
int WtDeleteHalfedgeFromTree(Vertex a, Vertex b, WtTreeNode *edges,
		     Edge *next_edge);

/* Duration functions. */
int WtElapsedTime (Vertex tail, Vertex head, WtNetwork *nwp);
void WtTouchEdge(Vertex tail, Vertex head, WtNetwork *nwp);

/* Utility functions. */
int WtFindithEdge (Vertex *tail, Vertex *head, double *weight, Edge i, WtNetwork *nwp);
int WtGetRandEdge(Vertex *tail, Vertex *head, double *weight, WtNetwork *nwp);
void Wtprintedge(Edge e, WtTreeNode *edges);
void WtInOrderTreeWalk(WtTreeNode *edges, Edge x);
void WtNetworkEdgeList(WtNetwork *nwp);
void WtShuffleEdges(Vertex *tails, Vertex *heads, double *weights, Edge nedges);

/* Others... */
Edge WtDesignMissing (Vertex a, Vertex b, WtNetwork *mnwp);
Edge WtEdgeTree2EdgeList(Vertex *tails, Vertex *heads, double *weights, WtNetwork *nwp, Edge nmax);

#endif
