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
  Vertex *vtails=NULL, *vheads=NULL;
  Network *nw;

  if (!isInteger(nedges) || !isInteger(nnodes) || !isInteger(directed_flag)
      || !isInteger(bipartite) || !isInteger(lasttoggle_flag) || !isInteger(time)
      || !isInteger(lasttoggle) ) return ans;

  if (INTEGER(nedges)[0] > 0){
    vtails = copyVertex(tails, INTEGER(nedges)[0]);
    vheads = copyVertex(heads, INTEGER(nedges)[0]);

    if (vtails == NULL || vheads == NULL){
      if (vtails != NULL) free(vtails);
      if (vheads != NULL) free(vheads);
      return ans;
    }
  }

  nw = NetworkInitialize(vtails, vheads, (Edge) INTEGER(nedges)[0],
        (Vertex) INTEGER(nnodes)[0], (int) INTEGER(directed_flag)[0], 
        (Vertex) INTEGER(bipartite)[0], (int) INTEGER(lasttoggle_flag)[0], 
        (int) INTEGER(time)[0], (int *) INTEGER(lasttoggle));

  PROTECT(ans = R_MakeExternalPtr(nw,install("edgetree ptr"), R_NilValue));
  R_RegisterCFinalizerEx(ans,edgetreeFinalizer, TRUE);

  if (vtails) free(vtails);
  if (vtails) free(vheads);

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
  Edge *eid;
  int *edge;

  PROTECT(ans = allocVector(INTSXP,2));
  edge = INTEGER(ans);
  edge[0] = edge[1] = 0L;

  if (!IS_EDGETREE_PTR(nw)) {
    UNPROTECT(1);
    return ans;
  } 

  eid = copyEdge(i,length(i)); 

  if (eid==NULL){
    UNPROTECT(1);
    return ans;
  }

  FindithEdge(edge,edge+1,eid[0], R_ExternalPtrAddr(nw));
  free(eid);
  UNPROTECT(1);
  return(ans);
}

SEXP GetEid_R(SEXP nw, SEXP tail, SEXP head){
  SEXP ans=R_NilValue;
  if (!IS_EDGETREE_PTR(nw)) {
    return ans;
  } 
  PROTECT(ans = allocVector(INTSXP,1));
  INTEGER(ans)[0] = GetEid(INTEGER(tail)[0],INTEGER(head)[0],R_ExternalPtrAddr(nw));
  UNPROTECT(1);
  return ans;
}

SEXP EdgetreeInfo_R(SEXP nw){
  SEXP ans=R_NilValue, nms;
  Network *pnw;

  if (!IS_EDGETREE_PTR(nw)) {
    return ans;
  } 

  pnw = (Network *)R_ExternalPtrAddr(nw);

  PROTECT(ans = allocVector(VECSXP, 5));
  PROTECT(nms = allocVector(STRSXP, 5));
  SET_STRING_ELT(nms, 0, mkChar("directed_flag"));
  SET_STRING_ELT(nms, 1, mkChar("bipartite"));
  SET_STRING_ELT(nms, 2, mkChar("nnodes"));
  SET_STRING_ELT(nms, 3, mkChar("nedges"));
  SET_STRING_ELT(nms, 4, mkChar("maxedges"));

  SET_VECTOR_ELT(ans, 0, ScalarInteger(pnw->directed_flag));
  SET_VECTOR_ELT(ans, 1, ScalarInteger(pnw->bipartite));
  SET_VECTOR_ELT(ans, 2, ScalarInteger(pnw->nnodes));
  SET_VECTOR_ELT(ans, 3, ScalarInteger(pnw->nedges));
  SET_VECTOR_ELT(ans, 4, ScalarInteger(pnw->maxedges));
  setAttrib(ans, R_NamesSymbol, nms);
  UNPROTECT(2);

  return ans;
}

struct Vertices {
  int i;
  Vertex *v;
};
int AdjacencyList_fn(TreeNode *tree, void *ctx){
  struct Vertices *v;

  v = (struct Vertices *)ctx;
  if (tree->value==0) return 0;
  v->v[v->i] = tree->value;
  (v->i)++;
  return 0;
}
Vertex *AdjacencyList(Network *nw, Edge x, int inout, int *len){
  struct Vertices v;

  *len = (inout==0) ?  nw->indegree[x] : nw->outdegree[x];

  v.i = 0;
  v.v = calloc(*len, sizeof(Vertex));

  if (inout==0)
    EdgeTreeWalk(nw->inedges, x, &AdjacencyList_fn, &v);
  else 
    EdgeTreeWalk(nw->outedges, x, &AdjacencyList_fn, &v);

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
    out = AdjacencyList(pnw,*pvertex,1, &outlen);
  if (t == 2 || t == 3)
    in = AdjacencyList(pnw,*pvertex, 0, &inlen);

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

SEXP EdgeTree2EdgeList_R(SEXP nw, SEXP eid){
  SEXP ans=R_NilValue, t;
  int *tails, *heads, *eids=NULL, cols=2;
  Network *pnw;

  if (!IS_EDGETREE_PTR(nw)) return ans;
  pnw = R_ExternalPtrAddr(nw);

  if (!isLogical(eid)) return ans;

  if (pnw->nedges == 0) return ans;

  if (LOGICAL(eid)[0])
    cols=3;

  PROTECT(ans = allocVector(INTSXP,pnw->nedges*cols));
  PROTECT(t = allocVector(INTSXP, 2));
  INTEGER(t)[0] = pnw->nedges;
  INTEGER(t)[1] = cols;

  tails = INTEGER(ans);
  heads = tails + pnw->nedges;

  if (LOGICAL(eid)[0]){
    eids = heads+pnw->nedges;
    EdgeTree2EdgeListWithEid(tails,heads,eids, pnw,pnw->nedges);
  } else {
    EdgeTree2EdgeList(tails,heads,pnw,pnw->nedges);
  }

  setAttrib(ans,R_DimSymbol, t);

  UNPROTECT(2);
  return ans;
  
}
