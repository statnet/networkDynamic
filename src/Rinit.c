/*
######################################################################
#
# utils.c
#
# Written by Jeffrey Horner <jeffrey.horner@gmail.com>
# Last Modified 7/08/2014
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/networkDynamic package
#
# This file contains the R/C initialization code
#
######################################################################
*/

#include <R.h>            
#include <Rinternals.h>            
#include <R_ext/Rdynload.h>
#include "spellfunctions.h"
#include "is.active.h"
#include "diagnostics.h"
#include "network_init.h"

#define CALLDEF(name, n) {#name,(DL_FUNC) &name, n}
static R_CallMethodDef CallEntries[] = {
  CALLDEF(ActivateEdges_R,5),
  CALLDEF(IsSpellActive_R,5),
  CALLDEF(IsActiveInVector_R,6),
  CALLDEF(InsertSpell_R,4),
  {NULL,NULL,0}
};

static R_CMethodDef CEntries[] = {
  CALLDEF(DurationMatrix,8),
  {NULL,NULL,0}
};

void R_init_networkDynamic(DllInfo *dll)
{
   R_registerRoutines(dll,CEntries,CallEntries, NULL, NULL);
   R_useDynamicSymbols(dll, FALSE);

   /* Callable functions from other packages' C code */
#define RREGDEF(name) R_RegisterCCallable("network", #name, (DL_FUNC) name)

   import_network_functions();
}
