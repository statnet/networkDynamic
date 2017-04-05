/*
 *  File networkDynamic/src/edgetree.h
 *  Part of the statnet package, http://statnetproject.org
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) in
 *    http://statnetproject.org/attribution
 *
 *  Copyright 2012 the statnet development team
 */
#ifndef EDGETREE_H
#define EDGETREE_H

#include <math.h>
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

#define MIN(a,b) ((a)<(b) ? (a) : (b))
#define MAX(a,b) ((a)<(b) ? (b) : (a))

/*typedef unsigned int Vertex;
typedef unsigned int Edge; */
typedef int Vertex;
typedef int Edge;



/*  TreeNode is a binary tree structure, which is how the edgelists 
    are stored.  The root of the tree for vertex i will be inedges[i]
    or outedges[i].  inedges[0] and outedges[0] are unused, since the
    index 0 will indicate no link.  Indices are long unsigned integers,
    which means networks can contain 2^32-1= 4294967295 edges (enough to
    hold all edges in a 92682-vertex undirected network or a 65536-vertex
    directed network, assuming no multiple edges or loops), though for this
    MAXEDGES must be adjusted accordingly.
*/
typedef struct TreeNodestruct {
  Vertex value;      /*  the vertex at the other end of this edge  */
  Edge parent;   /*  parent of this node in the tree (0 for root) */
  Edge left;     /*  left child (0 if none)  */
  Edge right;    /*  right child (0 if none) */
} TreeNode;

/* Dur_Inf is a structure containing information about durations of
edges in a network structure.
*/ 
typedef struct Dur_Infstruct {
  int MCMCtimer;
  int *lasttoggle;
} Dur_Inf;



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
*/
typedef struct Networkstruct {
  TreeNode *inedges;
  TreeNode *outedges;
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
} Network;


/* *** don't forget,  tails -> heads, so all the functions below using
   heads & tails, now list tails before heads */

/* Initialization and destruction. */
Network NetworkInitialize(Vertex *tails, Vertex *heads, Edge nedges,
			  Vertex nnodes, int directed_flag, Vertex bipartite,
			  int lasttoggle_flag, int time, int *lasttoggle);
void NetworkDestroy(Network *nwp);
Network NetworkInitializeD(double *tails, double *heads, Edge nedges,
			   Vertex nnodes, int directed_flag, Vertex bipartite,
			   int lasttoggle_flag, int time, int *lasttoggle);

Network *NetworkCopy(Network *dest, Network *src);

/* Accessors. */
Edge EdgetreeSearch (Vertex a, Vertex b, TreeNode *edges);
Edge EdgetreeSuccessor (TreeNode *edges, Edge x);
Edge EdgetreePredecessor (TreeNode *edges, Edge x);
Edge EdgetreeMinimum (TreeNode *edges, Edge x);
Edge EdgetreeMaximum (TreeNode *edges, Edge x);

/* Modifiers. */

/* *** don't forget,  tails -> heads, so all the functions below using
   heads & tails, now list tails before heads */

int ToggleEdge (Vertex tail, Vertex head, Network *nwp);
int ToggleEdgeWithTimestamp (Vertex tail, Vertex head, Network *nwp);
int AddEdgeToTrees(Vertex tail, Vertex head, Network *nwp);
void AddHalfedgeToTree (Vertex a, Vertex b, TreeNode *edges, Edge next_edge);
void UpdateNextedge (TreeNode *edges, Edge *nextedge, Network *nwp);
int DeleteEdgeFromTrees(Vertex tail, Vertex head, Network *nwp);
int DeleteHalfedgeFromTree(Vertex a, Vertex b, TreeNode *edges,
		     Edge *next_edge);

/* Duration functions. */
int ElapsedTime(Vertex tail, Vertex head, Network *nwp);
void TouchEdge(Vertex tail, Vertex head, Network *nwp);

/* Utility functions. */
int FindithEdge (Vertex *tail, Vertex *head, Edge i, Network *nwp);
int GetRandEdge(Vertex *tail, Vertex *head, Network *nwp);
void printedge(Edge e, TreeNode *edges);
void InOrderTreeWalk(TreeNode *edges, Edge x);
void NetworkEdgeList(Network *nwp);
void ShuffleEdges(Vertex *tails, Vertex *heads, Edge nedges);

/* Others... */
Edge DesignMissing (Vertex a, Vertex b, Network *mnwp);
Edge EdgeTree2EdgeList(Vertex *tails, Vertex *heads, Network *nwp, Edge nmax);

#endif
