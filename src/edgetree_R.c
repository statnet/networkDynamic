/*
 *  File networkDynamic/src/edgetree.c
 *  Part of the statnet package, http://statnetproject.org
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) in
 *    http://statnetproject.org/attribution
 *
 *  Copyright 2015 the statnet development team
 */
#include "edgetree_R.h"

static void edgetreeFinalizer( SEXP ptr ){
  if(!R_ExternalPtrAddr(ptr)) return;
  NetworkDestroy((Network *)R_ExternalPtrAddr(ptr));
  R_ClearExternalPtr(ptr);
}

static Vertex *copyVertex(SEXP svertices, Edge nedges){
  Vertex i, *vertices;

  if ((Edge)length(svertices) != nedges || !(isInteger(svertices) || isNumeric(svertices))) return NULL;

  vertices = calloc(nedges, sizeof(Vertex));

  if (isInteger(svertices)){
    Vertex *pvertices = INTEGER(svertices);
    for (i = 0; i < nedges; i++)
      vertices[i]  = (Vertex) pvertices[i];
  } else { /* isNumeric */
    double *pvertices = REAL(svertices);
    for (i = 0; i < nedges; i++)
      vertices[i]  = (Vertex) pvertices[i];
  }
  return vertices; 
}

static Edge *copyEdge(SEXP sedges, Edge nedges){
  Edge i, *edges;

  if ((Edge)length(sedges) != nedges || !(isInteger(sedges) || isNumeric(sedges))) return NULL;

  edges = calloc(nedges, sizeof(Edge));

  if (isInteger(sedges)){
    Vertex *pedges = INTEGER(sedges);
    for (i = 0; i < nedges; i++)
      edges[i]  = (Edge) pedges[i];
  } else { /* isNumeric */
    double *pedges = REAL(sedges);
    for (i = 0; i < nedges; i++)
      edges[i]  = (Vertex) pedges[i];
  }
  return edges; 
}

SEXP NetworkInitialize_R(SEXP tails, SEXP heads, SEXP nedges,
			  SEXP nnodes, SEXP directed_flag, SEXP bipartite,
			  SEXP lasttoggle_flag, SEXP time, SEXP lasttoggle){

  SEXP ans=R_NilValue;
  Vertex *vtails, *vheads;
  Network *nw;

  if (!isInteger(nedges) || !isInteger(nnodes) || !isInteger(directed_flag)
      || !isInteger(bipartite) || !isInteger(lasttoggle_flag) || !isInteger(time)
      || !isInteger(lasttoggle) ) return ans;

  vtails = copyVertex(tails, INTEGER(nedges)[0]);
  vheads = copyVertex(heads, INTEGER(nedges)[0]);

  if (vtails == NULL || vheads == NULL){
    if (vtails != NULL) free(vtails);
    if (vheads != NULL) free(vheads);
    return ans;
  }

  nw = NetworkInitialize(vtails, vheads, (Edge) INTEGER(nedges)[0],
        (Vertex) INTEGER(nnodes)[0], (int) INTEGER(directed_flag)[0], 
        (Vertex) INTEGER(bipartite)[0], (int) INTEGER(lasttoggle_flag)[0], 
        (int) INTEGER(time)[0], (int *) INTEGER(lasttoggle));

  PROTECT(ans = R_MakeExternalPtr(nw,install("edgetree ptr"), R_NilValue));
  R_RegisterCFinalizerEx(ans,edgetreeFinalizer, TRUE);

  free(vtails);
  free(vheads);

  UNPROTECT(1);
  return ans;
}

#define IS_EDGETREE_PTR(p) (R_ExternalPtrTag(p) == install("edgetree ptr"))

SEXP NetworkCopy_R(SEXP nw){
  SEXP ans=R_NilValue;
  Network *cnw;
  if (!IS_EDGETREE_PTR(nw)) return ans;

  cnw = (Network *) calloc(1, sizeof(Network));

  NetworkCopy(cnw, (Network *)R_ExternalPtrAddr(nw));

  PROTECT(ans = R_MakeExternalPtr(cnw,install("edgetree ptr"), R_NilValue));
  R_RegisterCFinalizerEx(ans,edgetreeFinalizer, TRUE);

  UNPROTECT(1);

  return ans;
}

SEXP NetworkEdgeList_R(SEXP nw){
  if (IS_EDGETREE_PTR(nw)) 
    NetworkEdgeList( (Network *) R_ExternalPtrAddr(nw));
  return R_NilValue;
}

SEXP AddDelEdgeToTrees_R( SEXP tail, SEXP head, SEXP nw, SEXP add){
  SEXP ans;
  Network *nwp;
  Vertex i, *vtails, *vheads;
  int len;

  PROTECT(ans = allocVector(LGLSXP, 1));
  LOGICAL(ans)[0] = FALSE;

  if (!IS_EDGETREE_PTR(nw)) {
    UNPROTECT(1);
    return ans;
  } 

  vtails = copyVertex(tail, (Edge) length(tail));
  vheads = copyVertex(head, (Edge) length(head));

  if (vtails == NULL || vheads == NULL){
    if (vtails != NULL) free(vtails);
    if (vheads != NULL) free(vheads);
    UNPROTECT(1);
    return ans;
  }

  len = length(tail);
  nwp = R_ExternalPtrAddr(nw);
  if (isLogical(add) && LOGICAL(add)[0] == TRUE)
    for(i = 0; i < len; i++) AddEdgeToTrees(vtails[i],vheads[i],nwp);
  else
    for(i = 0; i < len; i++) DeleteEdgeFromTrees(vtails[i],vheads[i],nwp);

  free(vtails);
  free(vheads);

  LOGICAL(ans)[0] = TRUE;
  UNPROTECT(1);

  return ans;
}

SEXP FindithEdge_R(SEXP i, SEXP nw){
  SEXP ans;
  Edge *edge;
  int *pans;

  PROTECT(ans = allocVector(INTSXP,2));
  pans = INTEGER(ans);
  pans[0] = pans[1] = 0L;

  if (!IS_EDGETREE_PTR(nw)) {
    UNPROTECT(1);
    return ans;
  } 

  edge = copyEdge(i,length(i)); 

  if (edge==NULL){
    UNPROTECT(1);
    return ans;
  }

  FindithEdge(pans,pans+1,edge[0], R_ExternalPtrAddr(nw));
  UNPROTECT(1);
  return(ans);
}

SEXP EdgetreeIsDirected_R(SEXP nw){
  SEXP ans;

  PROTECT(ans = allocVector(LGLSXP, 1));
  LOGICAL(ans)[0] = FALSE;

  if (!IS_EDGETREE_PTR(nw)) {
    UNPROTECT(1);
    return ans;
  } 

  if ( ((Network *)R_ExternalPtrAddr(nw))->directed_flag != 0)
    LOGICAL(ans)[0] = TRUE;

  UNPROTECT(1);
  return ans;
}

void EdgeTreeWalk(TreeNode *tree, Edge x, 
                  void (*fn)(TreeNode *tree, void *ctx), void *ctx){
  if (x == 0) return;

  EdgeTreeWalk(tree, (tree+x)->left, fn, ctx);
  if (fn) (*fn)(tree+x, ctx);
  EdgeTreeWalk(tree, (tree+x)->right, fn, ctx);
}

void TreeNodeCount_fn(TreeNode *tree, void *ctx){
  if (tree->value==0) return;
  (*(int *)ctx)++;
}
int TreeNodeCount(TreeNode *tree, Edge x){
  int sum=0;

  EdgeTreeWalk(tree,x,&TreeNodeCount_fn,&sum);

  return sum;
}

struct Vertices {
  int i;
  Vertex *v;
};
void Tree2Vertices_fn(TreeNode *tree, void *ctx){
  struct Vertices *v;

  v = (struct Vertices *)ctx;
  if (tree->value==0) return;
  v->v[v->i] = tree->value;
  (v->i)++;
}
Vertex *Tree2Vertices(TreeNode *tree, Edge x, int *len){
  struct Vertices v;
  *len = TreeNodeCount(tree, x);

  v.i = 0;
  v.v = calloc(*len, sizeof(Vertex));

  EdgeTreeWalk(tree, x, &Tree2Vertices_fn, &v);

  return v.v;
}

/* type: 1=out, 2=in, 3=combined */
SEXP GetNeighborhood_R(SEXP nw, SEXP vertex, SEXP type){
  SEXP ans=R_NilValue;
  Vertex *pvertex, *out=NULL, *in=NULL;
  Network *pnw;
  int t, outlen=0, inlen=0, i;

  if (!IS_EDGETREE_PTR(nw)) return ans;
  pnw = R_ExternalPtrAddr(nw);

  if (!isInteger(type)) return ans;
  t = INTEGER(type)[0];

  pvertex = copyVertex(vertex, 1);

  if (pvertex == NULL) return ans;

  if (!pnw->directed_flag) t = 3;

  if (t == 1 || t == 3)
    out = Tree2Vertices(pnw->outedges,*pvertex,&outlen);
  if (t == 2 || t == 3)
    in = Tree2Vertices(pnw->inedges,*pvertex, &inlen);

  free(pvertex);

  if ((outlen + inlen) == 0) return ans;

  PROTECT(ans = allocVector(INTSXP,outlen+inlen));
  i=0;
  if (out){
    for (t = 0; t < outlen; t++) INTEGER(ans)[i++] = out[t];
    free(out);
  }
  if (in){
    for (t = 0; t < inlen; t++) INTEGER(ans)[i++] = in[t];
    free(in);
  }

  UNPROTECT(1);
  return ans;

}
