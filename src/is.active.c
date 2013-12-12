/*
 *  File networkDynamic/src/is.active.c
 *  Part of the statnet package, http://statnetproject.org
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) in
 *    http://statnetproject.org/attribution
 *
 *  Copyright 2013 the statnet development team
 */
/*  This is a C helper function used by is.active() (access.R). */

#include "is.active.h"

/**********************************************************/      

/* Consider registering as native routine */
/* Function to determine if a query spell matches with any of the elements in a spell matrix 
  onset = onset of query spell
  terminus = terminus of query spell
  spell_list = spell matrix to be tested
  all = use all rule? (default to any)
*/
Rboolean IsSpellActive_int(double onset, double terminus, SEXP spell_list, Rboolean all, Rboolean debug_output) {
  SEXP spell_dim;
  PROTECT(spell_dim = getAttrib(spell_list, R_DimSymbol));
  int *dimp = INTEGER(spell_dim);
  int n_spells = dimp[0], spells_dim2 = dimp[1];
  UNPROTECT(1); /* spell_dim */
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

SEXP IsSpellActive (SEXP onset_s, SEXP terminus_s, SEXP spell_list, SEXP all_s, SEXP debug_output_s) {
  double onset = asReal(onset_s), terminus = asReal(terminus_s);
  Rboolean all = asLogical(all_s);
  Rboolean debug_output = asLogical(debug_output_s);
  if (onset == NA_REAL)    error("Bad value for 'onset'");
  if (terminus == NA_REAL) error("Bad value for 'terminus'");
  if (all == NA_LOGICAL)   error("Bad value for 'all'");
  if (debug_output == NA_LOGICAL)   error("Bad value for 'debug_output'");
  return ScalarLogical(IsSpellActive_int(onset, terminus, spell_list, all, debug_output));
}

/* Function to loop over a list of spell matrices (corresponding to vertices or edges) and determine 
for each element if it is active during the specified spell
 onset_s  = onset of query spell
 terminus_s = terminus of query spell
 spell_lists  = list of spellmatrices to evaluate
 all  = logical, use "all" rule? (matched spell must be active for entire duratin of query spell)
 active_default_s = logical, should elements with no spell matrix be considered active by default
 debug_output_s = logical, print debugging messages. 
*/
SEXP IsActiveInVector (SEXP onset_s, SEXP terminus_s, SEXP spell_lists, SEXP all_s, SEXP active_default_s, SEXP debug_output_s) {
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
      any[i] = IsSpellActive_int(onset, terminus, active_i_real, all, debug_output);
      if ( debug_output )
        Rprintf(": %c\n", any[i]? 'T':'F');
      UNPROTECT(1); /* active_i_real */
    }
    UNPROTECT(1); /* active_i */
  }
  UNPROTECT(1); /* any_s */
  return any_s;
}

