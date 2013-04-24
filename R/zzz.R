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
# .First.lib is run when the package is loaded with library(networkDynamic)
#
######################################################################

.First.lib <- function(lib, pkg){
    if(R.version$major=="1"){
     ehelp <- help(package="networkDynamic")$info[[2]][[2]]
     cat(paste("'",ehelp[4],"'\n",
               "Version ",ehelp[2],
               " created on ",ehelp[3],".\n", sep=""))
    }else{
     ehelp <- help(package="networkDynamic")$info[[1]]
     cat(paste(substring(ehelp[3],first=16),"\n",
               "Version ",substring(ehelp[4],first=16),
               " created on ",
                substring(ehelp[5],first=16),".\n", sep=""))
    }
    cat(paste("copyright (c) 2009, Carter T. Butts, University of California-Irvine\n",sep=""))
    cat('For citation information, type citation("networkDynamic").\n')
    cat('Type help("networkDynamic-package") to get started.\n')
    cat('NOTE: This is an EXPERIMENTAL PACKAGE.  Do not use unless you know\nwhat you are doing!\n')
}
