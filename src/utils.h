/*
 *  File networkDynamic/src/utils.h
 *  Part of the statnet package, http://statnetproject.org
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) in
 *    http://statnetproject.org/attribution
 *
 *  Copyright 2012 the statnet development team
 */
#ifndef UTILS_H 
#define UTILS_H

#include <R.h>
#include <Rinternals.h>

SEXP getListElement(SEXP list, const char *str);
SEXP setListElement(SEXP list, const char *str, SEXP elem);
SEXP enlargeList(SEXP list, int n);

#endif