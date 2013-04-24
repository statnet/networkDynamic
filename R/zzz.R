#  File networkDynamic/R/zzz.R
#  Part of the statnet package, http://statnetproject.org
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) in
#    http://statnetproject.org/attribution
#
#  Copyright 2012 the statnet development team
######################################################################
# .onLoad is run when the package is loaded with library(networkDynamic)
#
######################################################################

.onLoad<- function(lib, pkg){
    packageStartupMessage(mkStartupMessage('networkDynamic'))
    packageStartupMessage("Copyright (c) 2009 Carter T. Butts")
    packageStartupMessage('Type help("networkDynamic-package") to get started.')
}

# Functions below copied from statnet.common.  Putting it here for now because statnet.common is not on CRAN, so can't have the dependencey yet. 
mkStartupMessage <- function(pkgname){
  require(utils) # need this for packageDescription() and person() 
  INST_MAP <- list(washington.edu="University of Washington",
                   uw.edu="University of Washington",
                   psu.edu="Penn State University",
                   uci.edu="University of California -- Irvine",
                   ucla.edu="University of California -- Los Angeles",
                   nyu.edu="New York University") 

  desc <- packageDescription(pkgname)
  pns <- eval(parse(text=desc$`Authors@R`))
  pnnames <- format(pns, include=c("given","family"))
  pninsts <- sapply(pns, function(pn) NVL(INST_MAP[[gsub(".*?([^.@]+\\.[^.]{2,4})$","\\1",NVL(pn$email,""))]],""))

  authors <- sapply(pns, function(pn) "aut" %in% pn$role)

  pnlines <- ifelse(pninsts=="", pnnames, paste(pnnames,pninsts, sep=", "))
  
  copylist <- paste("Copyright (c) ",substr(desc$Date,1,4),", ",sep="")
  copylist <- paste(copylist, pnlines[authors][1],"\n",
                    paste(
                      paste(rep(" ",nchar(copylist)),collapse=""),
                      c(pnlines[authors][-1],if(sum(!authors)) "with contributions from",pnlines[!authors]),sep="",collapse="\n"),
                    sep="") 
     paste("\n",desc$Package,": version ", desc$Version, ', created on ', desc$Date, '\n',copylist,"\n",
          'Based on "statnet" project software (statnet.org).\n',
          'For license and citation information see statnet.org/attribution\n',
          'or type citation("',desc$Package,'").\n', sep="")
}

## If EXPR is NULL, return NULLV, otherwise return EXPR.
NVL <- function(EXPR, NULLV) if(!is.null(EXPR)) EXPR else NULLV