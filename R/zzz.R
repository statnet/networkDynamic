######################################################################
#
# zzz.R
#
# Written by Carter T. Butts <buttsc@uci.edu>.
#
# Last Modified 3/16/09
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/networkDynamic package
#
# .onLoad is run when the package is loaded with library(networkDynamic)
#
######################################################################

.onLoad<- function(lib, pkg){
  # load c library code
	#library.dynam("networkDynamic", package=pkgname, lib.loc=libname)
    if(R.version$major=="1"){
     ehelp <- (library(help="networkDynamic"))$info[[1]]
     packageStartupMessage(paste("'",ehelp[1],"'\n",
               "Version ",ehelp[4],
               " created on ",ehelp[3],".\n", sep=""))
    }else{
     ehelp <- (library(help="networkDynamic"))$info[[1]]
     packageStartupMessage("\n")
     packageStartupMessage(paste('Package ',substring(ehelp[1],first=16),': ',substring(ehelp[3],first=16),"\n",
               "Version ",substring(ehelp[4],first=16),
               " created on ",
                substring(ehelp[5],first=16),".\n", sep=""))
    }
    packageStartupMessage(paste("Copyright (c) 2012, The Stanet Project (statnet.org), \n (c) 2009, Carter T. Butts, University of California-Irvine\n",sep=""))
    packageStartupMessage('For citation information, type citation("networkDynamic").')
    packageStartupMessage('Type help("networkDynamic-package") to get started.')
    packageStartupMessage('NOTE: This is an initial public release of an EXPERIMENTAL PACKAGE which contains known bugs.')
}
