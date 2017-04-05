/*
 *  File networkDynamic/src/infection.h
 *  Part of the statnet package, http://statnetproject.org
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) in
 *    http://statnetproject.org/attribution
 *
 *  Copyright 2012 the statnet development team
 */
#ifndef INFECTION_H 
#define INFECTION_H

#include <R.h>
#include "wtedgetree.h"

/* Function prototypes */
void Prevalence (int *nnodes,
      int *nedge, int *edge, int *ntimestep, int *nfem, int *nseeds,
      int *ntotal, int *nchange, int *change, int *ndissolve, int *dissolve,
      int *randomseeds, double *betarate, int *infected, int *totinfected,
      int *nsim, int *prev);
void PrevalenceWithBernoulliOption(int *nnodes,
      int *nedge, int *edge, int *ntimestep, int *nfem,
      int *ntotal, int *nchange, int *change, int *ndissolve, int *dissolve,
      int *bernoulli, double *betarate, int *infected, int *nsim, int *prev);
      
#endif

