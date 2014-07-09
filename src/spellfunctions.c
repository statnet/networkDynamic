/*
 *  File networkDynamic/src/spellfunctions.c
 *  Part of the statnet package, http://statnetproject.org
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) in
 *    http://statnetproject.org/attribution
 *
 *  Copyright 2012 the statnet development team
 */
/*  This is a C helper function used by is.active() (access.R). */

#include <Rmath.h>
#include "spellfunctions.h"
#include "network.h"

typedef struct {
  double *onsets;
  double *termini;
}  spell_list_internal_t;

SEXP InsertSpell(SEXP spell_list, double onset, double terminus, Rboolean debug_output) {
  SEXP spell_dim;
  int *dimp;
  int n_old_spells, spells_dim2, n_new_spells;
  int s, t, first_to_copy_before = 0, last_to_copy_before = -1, 
    first_to_copy_after, last_to_copy_after;
  spell_list_internal_t old_spells, new_spells;
  SEXP new_spell_list;

  if (isNull(spell_list)) {
    n_old_spells = 0;
  } else {
    PROTECT(spell_dim = getAttrib(spell_list, R_DimSymbol));
    dimp = INTEGER(spell_dim);
    n_old_spells = dimp[0];
    spells_dim2 = dimp[1];
    UNPROTECT(1); /* spell_dim */
    if ( spells_dim2 != 2 )    error("Misshapen matrix in 'spell_list'");
    old_spells.onsets = REAL(spell_list);
    old_spells.termini = old_spells.onsets + n_old_spells;
  }

  if ( debug_output ) {
    Rprintf("Insert [%g,%g) into", onset, terminus);
    for (s = 0; s < n_old_spells; ++s)
      Rprintf( " [%g, %g)", old_spells.onsets[s], old_spells.termini[s]);
    Rprintf("\n");
  }

  first_to_copy_after = n_old_spells;
  last_to_copy_after = n_old_spells - 1;
  for (s = 0; s < n_old_spells; ++s) {
    // (-Inf, -Inf) is a null spell, only valid when alone
    if ( old_spells.termini[s] == R_NegInf ) {
      if ( debug_output )
        Rprintf("Drop [%g,%g)\n",
          old_spells.onsets[s], old_spells.termini[s]);
      first_to_copy_before = s + 1;
    }
    // Any spell that's separate from our interval to the left, we copy.
    // Also if our interval is its open endpoint.
    else if ( old_spells.termini[s] < onset ||
         ( old_spells.onsets[s] < onset && onset == terminus &&
           terminus == old_spells.termini[s] ) ) {
      if ( debug_output )
        Rprintf("Keep [%g,%g) as is\n",
          old_spells.onsets[s], old_spells.termini[s]);
      last_to_copy_before = s;
    }
    // (Inf, Inf) is a null spell, only valid when alone
    else if ( old_spells.onsets[s] == R_PosInf ) {
      if ( debug_output )
        Rprintf("Drop [%g,%g)\n",
          old_spells.onsets[s], old_spells.termini[s]);
      last_to_copy_after = s - 1;
      first_to_copy_after = s; // see comment below
      break;
    }
    // Any spell that's separate from our interval to the right, we copy.
    // Also if it's our interval's open endpoint.
    else if ( terminus < old_spells.onsets[s] ||
              ( onset < terminus && terminus == old_spells.onsets[s]
                && old_spells.onsets[s] == old_spells.termini[s] ) ) {
      if ( debug_output )
        Rprintf("Keep [%g,%g) as is\n",
          old_spells.onsets[s], old_spells.termini[s]);
      first_to_copy_after = s;
      break;
    }
    // Any other spell merges with our interval.
    else {
      if ( debug_output ) {
        Rprintf("Merge [%g,%g) with new spell\n",
          old_spells.onsets[s], old_spells.termini[s]);
      }
    }
  }

  // At this point:
  // We will retain all spells from first_to_copy_before..last_to_copy_before
  // and first_to_copy_after..last_to_copy_after, both ranges inclusive of
  // endpoints, if the difference is nonnegative.
  // Any spells between last_to_copy_before and first_to_copy_after are
  // to be merged with the new spell.  This is why we have to adjust
  // both of the "after" indexes when dropping one from the end.
  
  // Now that we know what to do with all spells, we can build the new spell list.
  
  // Special case if spell list won't actually be modified
  if ( first_to_copy_before == 0 &&
       last_to_copy_after == n_old_spells - 1 &&
       last_to_copy_before + 2 == first_to_copy_after &&
       old_spells.onsets[ last_to_copy_before + 1 ] <= onset &&
       terminus <= old_spells.termini[ last_to_copy_before + 1 ] &&
       ! ( old_spells.onsets[ last_to_copy_before + 1 ] < onset &&
           onset == terminus &&
           terminus == old_spells.termini[ last_to_copy_before + 1 ] ) )
    return spell_list;

  n_new_spells = 1;
  if ( first_to_copy_before <= last_to_copy_before )
    n_new_spells += 1 + last_to_copy_before - first_to_copy_before;
  if ( first_to_copy_after <= last_to_copy_after )
    n_new_spells += 1 + last_to_copy_after - first_to_copy_after;
  PROTECT( new_spell_list = allocMatrix( REALSXP, n_new_spells, 2 ) );
  new_spells.onsets = REAL(new_spell_list);
  new_spells.termini = new_spells.onsets + n_new_spells;

  if ( debug_output )
    Rprintf( "copying %d - %d and %d - %d:", first_to_copy_before, last_to_copy_before,
      first_to_copy_after, last_to_copy_after);

  for (s = first_to_copy_before, t = 0; s <= last_to_copy_before; ++s, ++t) {
    new_spells.onsets[t] = old_spells.onsets[s];
    new_spells.termini[t] = old_spells.termini[s];
    if ( debug_output )
      Rprintf(" [%g,%g)", new_spells.onsets[t], new_spells.termini[t]);
  }
  if ( s < first_to_copy_after ) {
    //if ( debug_output )
    //  Rprintf("%d - %d: [%g,%g)\n", s, first_to_copy_after - 1,
    //    old_spells.onsets[s], old_spells.termini[ first_to_copy_after - 1 ]);
    onset = fmin2( onset, old_spells.onsets[s] );
    terminus = fmax2( terminus, old_spells.termini[ first_to_copy_after - 1 ] );
    if ( debug_output )
      Rprintf(" [%g,%g) (merge)", onset, terminus);
  }
  else if ( debug_output )
    Rprintf(" [%g,%g) (copy)", onset, terminus);
  new_spells.onsets[t] = onset;
  new_spells.termini[t] = terminus;
  for (s = first_to_copy_after, ++t; s <= last_to_copy_after; ++s, ++t) {
    new_spells.onsets[t] = old_spells.onsets[s];
    new_spells.termini[t] = old_spells.termini[s];
    if ( debug_output )
      Rprintf(" [%g,%g)", new_spells.onsets[t], new_spells.termini[t]);
  }
  if ( debug_output )
    Rprintf("\n");
  UNPROTECT(1);
  return new_spell_list;
}

SEXP InsertSpell_R(SEXP spell_list, SEXP onset_s, SEXP terminus_s, SEXP debug_output_s) {
  double onset = asReal(onset_s), terminus = asReal(terminus_s);
  Rboolean debug_output = asLogical(debug_output_s);
  if (onset == NA_REAL)    error("Bad value for 'onset'");
  if (terminus == NA_REAL) error("Bad value for 'terminus'");
  if (debug_output == NA_LOGICAL)   error("Bad value for 'debug_output'");
  if (terminus < onset)    error("Onset can't exceed terminus");
  return InsertSpell(spell_list, onset, terminus, debug_output);
}

/**********************************************************/      
/*    activate.edges() functions                          */
/**********************************************************/

SEXP ActivateEdges_R(SEXP network, SEXP onset, SEXP terminus, SEXP e, SEXP debug_output_s) {
  int pc=0, lpc;
  int *ep, n, i;
  double *onsets, *termini;
  Rboolean debug_output = asLogical(debug_output_s);
  SEXP mel, el, atl, active, new_active;
  // need to induce copy of network object so we don't modify the wrong thing
  network=duplicate(network);
  if ( debug_output )
    Rprintf("ActivateEdges_R\n");

  PROTECT(e = coerceVector(e,INTSXP)); ++pc;
  PROTECT(onset = coerceVector(onset,REALSXP)); ++pc;
  PROTECT(terminus = coerceVector(terminus,REALSXP)); ++pc;
  PROTECT(mel = getListElement(network,"mel")); ++pc;
  n = length(e);
  ep = INTEGER(e);
  onsets = REAL(onset);
  termini = REAL(terminus);

  if ( debug_output )
    Rprintf("%d edges\n", n);

  // for each valid edge in e
  for (i=0; i<n; ++i) {
    /* ignore (Inf,Inf) and (-Inf,-Inf) */
    if (onsets[i] == R_PosInf || termini[i] == R_NegInf)
      continue;
    lpc = 0;
    if ( debug_output )
      Rprintf("before get active\n");
    PROTECT(el = VECTOR_ELT(mel, ep[i]-1)); ++lpc;
    PROTECT(atl = getListElement(el,"atl")); ++lpc;
    /* We trust the calling R code to ensure that all edges passed in
       do exist.  The network package guarantees that all existing edges
       have the atl element; so we don't have to check el and atl for nil. 
       The active attribute here may be nil, on the other hand, which
       is okay. */
    PROTECT(active = getListElement(atl,"active")); ++lpc;
    
    if ( debug_output )
      Rprintf("before InsertSpell\n");
    PROTECT(new_active = InsertSpell(active, onsets[i], termini[i], debug_output)); ++lpc;
    if ( debug_output ) {
      Rprintf("before set active\n");
    }
    PROTECT(atl=setListElement(atl,"active",new_active)); ++lpc;
    /* see comment in network's setEdgeAttrib_R() about el not being modified */
    setListElement(el,"atl",atl);
    
    if ( debug_output )
      Rprintf("after\n");
    UNPROTECT(lpc);
  }
  UNPROTECT(pc);

  return network;
}
