/*
 *  File networkDynamic/src/diagnostics.c
 *  Part of the statnet package, http://statnetproject.org
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) in
 *    http://statnetproject.org/attribution
 *
 *  Copyright 2012 the statnet development team
 */
/*  This is a collection of functions used to calculate diagnostic 
    statistics for dynamic networks. */

#include "diagnostics.h"
#include<limits.h>

/* These #defines are not really necessary but may make the code a bit
   easier to read.  They come at a price (the possibility of hard-to-track
   errors).   */
#define DMATRIX(a,b) (dmatrix[(a)+(offset)*(b)])
#define EDGE(a,b) (edges[(a)+(*n_edges)*(b)])
#define CHANGE(a,b) (changes[(a)+(*n_changes)*(b)])

/* Helper functions defined inline. */

R_INLINE void AddNewDurationRow (int *dmatrix, int row, int t, int h, int time, int offset, int def_end, int left_censored) {
  DMATRIX(row, 0) = time; /* timestamp: edge begins */
  DMATRIX(row, 1) = def_end;    /* simulation end time */
  DMATRIX(row, 2) = t;    /* tail node number */
  DMATRIX(row, 3) = h;    /* head node number */
  DMATRIX(row, 4) = left_censored;    /* left-censoring */
  DMATRIX(row, 5) = 1;    /* right-censoring indicator:  1=censored, 0=not */
}

/**********************************************************/      


void DurationMatrix (int *n,  int *n_edges, int *edges, int *start, int *end,
      int *n_changes, int *changes,
      int *dmatrix) {
  int row, j, k, t, h, time, offset = *n_edges + *n_changes;

  /* Note:  This code assumes always that edges are listed in
     (tail, head) order, where, for bipartite and underected networks, tail < head.  */
  
  /* First, initialize dmatrix by putting in time-zero network */
  for (row=0; row<*n_edges; row++) {
    AddNewDurationRow (dmatrix, row, EDGE(row,0), EDGE(row,1), *start, offset, *end, 1);
  } /* Note:  Value of i upon leaving loop is important */

  /* Next, step through time one click at a time */
  for (time=*start,j=0; time<=*end; time++) {
    for(; CHANGE(j,0) == time && j<*n_changes; j++) {
      t = CHANGE(j,1);
      h = CHANGE(j,2);
      for(k=row; !(DMATRIX(k, 2)==t && DMATRIX(k, 3)==h) && k>=0; k--);
      if (k>=0 && DMATRIX(k,5) == 1) { 
        /* We found a match for the (t, h) edge that must be ended */
        DMATRIX(k, 1) = time;
        DMATRIX(k, 5) = 0; /* Censored indicator */
      } else {
        AddNewDurationRow(dmatrix, row++, t, h, time, offset, *end, 0);
      }
    }
  }
}


