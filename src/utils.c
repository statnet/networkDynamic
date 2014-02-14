/*
 *  File networkDynamic/src/utils.c
 *  Part of the statnet package, http://statnetproject.org
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) in
 *    http://statnetproject.org/attribution
 *
 *  Copyright 2012 the statnet development team
 */

/* Some list util functions,  borrowed from R documentation,
   and from the network package's utils.c */
#include "utils.h"

SEXP getListElement(SEXP list, const char *str)
/*Given a list and a character string, return a pointer to the element with the specified name (or else NULL).  This is taken from the Writing R Extensions manual.*/
{
  SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
  int i;

  for (i = 0; i < length(list); i++)
    if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      elmt = VECTOR_ELT(list, i);
      break;
    }
  return elmt;
}

SEXP setListElement(SEXP list, const char *str, SEXP elem)
/*Given a list, an element, and a character string, write the element with the specified name to the list.  If an element by that name already exists, it is replaced.*/
{
  SEXP names, newlist;
  int i, pc=0;

  /*If list is null or of zero length, we immeadiately know what to do*/
  if(length(list)==0){
    PROTECT(newlist=allocVector(VECSXP,1)); pc++;
    SET_VECTOR_ELT(newlist,0,elem);
    PROTECT(names=allocVector(STRSXP,1)); pc++;
    SET_STRING_ELT(names,0,mkChar(str));
    setAttrib(newlist,R_NamesSymbol,names);
    UNPROTECT(pc);
    return newlist;
  }

  /*Rprintf("\t\tEntered setListElement looking for %s.\n",str);*/
  /*Try to use an existing element, if possible*/
  names = getAttrib(list, R_NamesSymbol);
  for (i = 0; i < length(list); i++)
    if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      SET_VECTOR_ELT(list, i, elem);
      /*Rprintf("\t\t\tUsing existing entry (%d)\n",i);*/
      return list;
    }

  /*If not possible, add as a new element*/
  /*Rprintf("\t\t\tNot found; creating new entry\n");*/
  PROTECT(newlist = enlargeList(list,1)); pc++;
  SET_VECTOR_ELT(newlist,length(newlist)-1,elem);
  PROTECT(names = getAttrib(newlist, R_NamesSymbol)); pc++;
  SET_STRING_ELT(names,length(newlist)-1,mkChar(str));
  setAttrib(newlist,R_NamesSymbol,names);

  UNPROTECT(pc);
  return newlist;
}

SEXP enlargeList(SEXP list, int n)
/*Return a pointer to an enlarged version of list, where the length is increased by n steps.*/
{
  int i,pc=0;
  SEXP newlist=R_NilValue, names, newnames;

  /*Rprintf("\t\tenlargeList entered, extending length %d by %d\n",length(list),n);*/
  if(n>0){
    switch(TYPEOF(list)){
      case VECSXP:
        PROTECT(newlist = allocVector(VECSXP, length(list)+n)); pc++;
        PROTECT(newnames = allocVector(STRSXP, length(list)+n)); pc++;
        names = getAttrib(list, R_NamesSymbol);
        for(i=0;i<length(list);i++){
          SET_VECTOR_ELT(newlist,i,VECTOR_ELT(list,i));
          if(length(names)>i)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
        }
        if(length(names)>0)
          setAttrib(newlist,R_NamesSymbol,newnames);
        break;
      case STRSXP:
        PROTECT(newlist = allocVector(STRSXP, length(list)+n));  pc++;
        PROTECT(newnames = allocVector(STRSXP, length(list)+n)); pc++;
        names = getAttrib(list, R_NamesSymbol);
        for(i=0;i<length(list);i++){
          SET_STRING_ELT(newlist,i,STRING_ELT(list,i));
          if(length(names)>i)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
        }
        if(length(names)>0)
          setAttrib(newlist,R_NamesSymbol,newnames);
        break;
      case INTSXP:
        PROTECT(newlist = allocVector(INTSXP, length(list)+n));  pc++;
        PROTECT(newnames = allocVector(STRSXP, length(list)+n)); pc++;
        names = getAttrib(list, R_NamesSymbol);
        for(i=0;i<length(list);i++){
          INTEGER(newlist)[i]=INTEGER(list)[i];
          if(length(names)>i)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
        }
        if(length(names)>0)
          setAttrib(newlist,R_NamesSymbol,newnames);
        break;
      case REALSXP:
        PROTECT(newlist = allocVector(REALSXP, length(list)+n));  pc++;
        PROTECT(newnames = allocVector(STRSXP, length(list)+n)); pc++;
        names = getAttrib(list, R_NamesSymbol);
        for(i=0;i<length(list);i++){
          REAL(newlist)[i]=REAL(list)[i];
          if(length(names)>i)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
        }
        if(length(names)>0)
          setAttrib(newlist,R_NamesSymbol,newnames);
        break;
      case RAWSXP:
        PROTECT(newlist = allocVector(RAWSXP, length(list)+n));  pc++;
        PROTECT(newnames = allocVector(STRSXP, length(list)+n)); pc++;
        names = getAttrib(list, R_NamesSymbol);
        for(i=0;i<length(list);i++){
          RAW(newlist)[i]=RAW(list)[i];
          if(length(names)>i)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
        }
        if(length(names)>0)
          setAttrib(newlist,R_NamesSymbol,newnames);
        break;
      case LGLSXP:
        PROTECT(newlist = allocVector(LGLSXP, length(list)+n));  pc++;
        PROTECT(newnames = allocVector(STRSXP, length(list)+n)); pc++;
        names = getAttrib(list, R_NamesSymbol);
        for(i=0;i<length(list);i++){
          INTEGER(newlist)[i]=INTEGER(list)[i];
          if(length(names)>i)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
        }
        if(length(names)>0)
          setAttrib(newlist,R_NamesSymbol,newnames);
        break;
      default:
        error("unimplemented type in enlargeList\n");
    }
    UNPROTECT(pc);
    return newlist;
  }else{
    return list;
  }
}
