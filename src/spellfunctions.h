/*
 *  File networkDynamic/src/spellfunctions.h
 *  Part of the statnet package, http://statnetproject.org
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) in
 *    http://statnetproject.org/attribution
 *
 *  Copyright 2012 the statnet development team
 */
#ifndef SPELLFUNCTIONS_H 
#define SPELLFUNCTIONS_H

#include <R.h>
#include <Rinternals.h>

// replaces most of is.active()
SEXP IsActiveInVector_R(SEXP onset_s, SEXP terminus_s, SEXP spell_lists, SEXP all_s, SEXP active_default_s, SEXP debug_output);

// replaces most of insert.spell()
SEXP InsertSpell_R(SEXP spell_list, SEXP onset_s, SEXP terminus_s, SEXP debug_output);

// replaces most of activate.edges()
void ActivateEdges_R(SEXP network, SEXP onset, SEXP terminus, SEXP e, SEXP debug_output);

#endif
