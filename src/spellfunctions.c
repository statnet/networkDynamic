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

#include "spellfunctions.h"

/* Consider registering some of these as native routines? */

/**********************************************************/      
/*    is.active() functions                               */
/**********************************************************/      

Rboolean IsSpellActive(double onset, double terminus, SEXP spell_list, Rboolean all, Rboolean debug_output) {
  SEXP spell_dim;
  PROTECT(spell_dim = getAttrib(spell_list, R_DimSymbol));
  int *dimp = INTEGER(spell_dim);
  int n_spells = dimp[0], spells_dim2 = dimp[1];
  UNPROTECT(1); /* spell_dim */
  if ( spells_dim2 != 2 )    error("Misshapen matrix in 'spell_list'");
  double *onsets = REAL(spell_list);
  double *termini = onsets + n_spells;

  if (onsets[0] == R_NegInf && termini[0] == R_PosInf)
    return TRUE;

  for (int s = n_spells-1; s >= 0; --s) {
    if ( debug_output )
      Rprintf("Test [%g,%g) against [%g,%g) ",
        onset, terminus, onsets[s], termini[s]);
    if (onsets[s] == R_PosInf)
      continue;
    if (onset == terminus) { /* point query */
      if (onsets[s] == termini[s]) {
        if (onset == onsets[s])
          return TRUE;
      } else if (termini[s] == R_PosInf && onset == R_PosInf)
        return TRUE;
      else if (onsets[s] <= onset && onset < termini[s])
        return TRUE;
    } else { /* non-point query */
      if (all) {
        /* test whether query is completely within spell */
        /* this can produce false negative if spell_list isn't collapsed into minimal form */
        if (onsets[s] <= onset && terminus <= termini[s])
          return TRUE;
      } else {
        if (onsets[s] == termini[s]) {
          if (onset <= onsets[s] && onsets[s] < terminus)
            return TRUE;
        } else if (onset < termini[s] && onsets[s] < terminus)
          return TRUE;
      }
    }
  }
  /* failed to find sufficient overlap */
  return FALSE;
}

SEXP IsSpellActive_R(SEXP onset_s, SEXP terminus_s, SEXP spell_list, SEXP all_s, SEXP debug_output_s) {
  double onset = asReal(onset_s), terminus = asReal(terminus_s);
  Rboolean all = asLogical(all_s);
  Rboolean debug_output = asLogical(debug_output_s);
  if (onset == NA_REAL)    error("Bad value for 'onset'");
  if (terminus == NA_REAL) error("Bad value for 'terminus'");
  if (all == NA_LOGICAL)   error("Bad value for 'all'");
  if (debug_output == NA_LOGICAL)   error("Bad value for 'debug_output'");
  return ScalarLogical(IsSpellActive(onset, terminus, spell_list, all, debug_output));
}

SEXP IsActiveInVector_R(SEXP onset_s, SEXP terminus_s, SEXP spell_lists, SEXP all_s, SEXP active_default_s, SEXP debug_output_s) {
  double onset = asReal(onset_s), terminus = asReal(terminus_s);
  Rboolean all = asLogical(all_s);
  Rboolean active_default = asLogical(active_default_s);
  Rboolean debug_output = asLogical(debug_output_s);
  if (onset == NA_REAL)    error("Bad value for 'onset'");
  if (terminus == NA_REAL) error("Bad value for 'terminus'");
  if (all == NA_LOGICAL)   error("Bad value for 'all'");
  if (active_default == NA_LOGICAL) error("Bad value for 'active_default'");
  if (debug_output == NA_LOGICAL)   error("Bad value for 'debug_output'");

  SEXP any_s;
  PROTECT(any_s = allocVector(LGLSXP, length(spell_lists)));
  int *any = INTEGER(any_s);
  /* does this need to compile in more fussy C compilers? */
  for (int i = length(spell_lists) - 1; i >= 0; --i) {
    SEXP active_i;
    PROTECT(active_i = VECTOR_ELT(spell_lists, i));
    if (isNull(active_i))
      any[i] = active_default;
    else {
      SEXP active_i_real;
      PROTECT(active_i_real = coerceVector(active_i, REALSXP));
      any[i] = IsSpellActive(onset, terminus, active_i_real, all, debug_output);
      if ( debug_output )
        Rprintf(": %c\n", any[i]? 'T':'F');
      UNPROTECT(1); /* active_i_real */
    }
    UNPROTECT(1); /* active_i */
  }
  UNPROTECT(1); /* any_s */
  return any_s;
}

/**********************************************************/      
/*    insert.spell() functions                            */
/**********************************************************/      

#include <Rmath.h>

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
#include "utils.h"

void ActivateEdges_R(SEXP network, SEXP onset, SEXP terminus, SEXP e, SEXP debug_output_s) {
  int pc=0, lpc;
  int *ep, n, i, *e_i_v_p;
  double *onsets, *termini;
  Rboolean debug_output = asLogical(debug_output_s);
  SEXP mel, el, atl, active, new_active, e_i_vector, active_vector;
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
}
