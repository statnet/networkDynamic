/*
 *  File networkDynamic/src/diagnostics.h
 *  Part of the statnet package, http://statnetproject.org
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) in
 *    http://statnetproject.org/attribution
 *
 *  Copyright 2012 the statnet development team
 */
#ifndef IS_ACTIVE_H 
#define IS_ACTIVE_H

#include <R.h>
#include <Rinternals.h>

/* Function prototypes */
SEXP IsSpellActive_R (SEXP onset_s, SEXP terminus_s, SEXP spell_list, SEXP all_s, SEXP debug_output);

SEXP IsActiveInVector_R (SEXP onset_s, SEXP terminus_s, SEXP spell_lists, SEXP all_s, SEXP active_default_s, SEXP debug_output);

#endif
