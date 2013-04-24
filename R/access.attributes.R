####################################################################
#
# This was my start to writing TEA functions.  It was written probably
# a full year ago (around summer 2011) and as such is unlikely to
# match the current spec
#
# It also was never fully completed, in the sense that there were
# lingering questions about TEA specs
#
# Hopefully someone will find it useful -- alc
#
####################################################################

# Skye:  I commented these out for now because this insert.spell method was conflicting with the one in access.R

# activate.edge.attribute <- function(x, prefix, value, onset=-Inf,
#                                     terminus=NULL, e=1:length(x$mel),
#                                     dynamic.only=FALSE) {
# 
#   ### error-checking code
#   if(!is.network(x))   # single breakage:
#     stop("activate.edge.attribute requires an argument of class network.\n")
# 
#   if(!is.character(prefix))
#     stop("prefixes must be character strings in activate.edge.attribute.\n")
#   
#   if(length(prefix) > 1)
#     warning("Only the first element of prefix will be used.\n")
#   
#   if(!is.vector(value) && !is.list(value))
#     stop("Inappropriate value given in activate.edge.attribute.\n")
#   
#   if(!is.vector(onset) || !is.numeric(onset))
#     stop("Onset times must be a numeric vector in activate.edge.attribute.\n")
# 
#   if(!is.null(terminus) && (!is.vector(terminus) || !is.numeric(terminus)))
#     stop("Terminus times must be a numeric vector in activate.edge.attribute.\n")
# 
#   if(!is.vector(e) || !is.numeric(e))
#     stop("Edge ID's, e, must be a numeric vector in activate.edge.attribute.\n")
#      
#   if(!is.logical(dynamic.only))
#     stop("dynamic.only flag must be a logical in activate.edge.attribute.\n")
# 
#   if((min(e) < 1) || (max(e) > length(x$mel)))   # combinatorial breakage:
#     stop("Illegal edge in activate.edge.attribute.\n")
# 
#   e <- e[!sapply(x$mel[e], is.null)]  #Filter out non-edges
#   if(length(e)==0)  return()
#   value <- as.list(rep(value, length=length(e)))
#   onset <- rep(onset, length=length(e))
#   terminus <- rep(terminus, length=length(e))
# 
#   if(!is.null(terminus) && any(onset > terminus))
#     stop("Onset times must precede terminous times in activate.edge.attribute.\n")
# 
#   # Get existing TEAs (and regular attributes if !'dynamic.only')
#   tea <- paste(prefix, ".active", sep="")
#   active <- lapply(lapply(x$mel[e], "[[", "atl"), "[[", tea)
#   null.active <- sapply(active, is.null)
#   if(any(null.active) & !dynamic.only) {
#     prefix.vals <- lapply(lapply(x$mel[e], "[[", "atl"), "[[", prefix)
#     active[null.active] = prefix.vals[null.active]
#     prefixed <- null.active & !sapply(active, is.null)
#   }
#     
#   for(i in seq_along(active)) {
# 
#     # current values and spell matrices..
#     if(prefixed[i]) { # found 'prefix', not 'prefix.active'
#       spells <- matrix(c(-Inf, Inf), 1, 2)
#       values <- active[[i]]
#     } else {
#       spells <- active[[i]][[2]]
#       values <- active[[i]][[1]] 
#     }
# 
#     # next decide what the terminus value should be if missing
#     if(is.null(terminus[i]) || is.na(terminus[i])) {
#       if(is.null(spells) || (is.infinite(spells[1,1]) & spells[1,1] > 0) ||
#          onset[i] >= spells[NROW(spells), 1]) {
#         terminus[i] = Inf
#       } else {
#         next.spell = min(which(onset[i] < spells[,1]))
#         terminus[i] = spells[next.spell, 1]
#       }
#     } 
#     active[[i]] <- insert.spell(values, spells, value[[i]], onset[i], terminus[i])
#   }
#   
#   xn <- deparse(substitute(x))
#   ev <- parent.frame()
#   set.edge.attribute(x, tea, active, e)
#   # ANNA-BELLE, NEED TO RESOLVE THIS POINT 
#   # delete 'prefix' attributes that became 'prefix.active' TEAs
#   # this is trickier than I thought, since I need to access mel$atl$prefix,
#   # only 'prefix' should be evaluated.
#   if(exists(xn, envir=ev))
#     on.exit(assign(xn, x, pos=ev))
#   invisible(x)  
# }
# 
# 
# 
# 
# #--------------------------------------------------------------
# # this is a helper function to insert a single spell
# #
# # @param
# #    cur.vals  : a list of current values
# #    cur.spells: the 2x(number of spells) matrix of current
# #                spells
# #    val       : the value of the spell to be inserted
# #    onset     : the onset time of the spell to be inserted;
# #                default=-Inf
# #    terminus  : the terminus time of the spell to be inserted;
# #                default=Inf
# #
# # @return:
# #    the list of two elements, 1) the updated values, 2) the
# #    updated spells
# #------------------------------------------------------------------
# insert.spell<-function(cur.vals, cur.spells, val, onset=-Inf,
#                        terminus=Inf){
#      if (is.null(cur.spells) ||
#          (is.infinite(cur.spells[1,1]) & cur.spells[1,1] > 0)) {
#        new.values <- list(val)
#        new.spells <- matrix(c(onset, terminus), 1,2)
#      } else {
#         double.entry=FALSE
#         ns = NROW(cur.spells)
# 	# find the row that this spell will go into
# 	if(onset>cur.spells[ns,1]) {
# 	  spell.row = ns+1
# 	} else {
# 	  spell.row = min(which(onset<=cur.spells[,1]))
# 	}
# 	# if the onset interrupts/continues an existing spell... 
# 	if (spell.row > 1 && onset<=cur.spells[spell.row-1,2]) {
#           if(identical(val, cur.vals[[spell.row-1]])) {
# 	     spell.row = spell.row-1
# 	     onset = cur.spells[spell.row,1]   # back up onset to that of interrupted spell
# 	   } else {
#              if(terminus < cur.spells[spell.row-1,2]){  # this spells splits the interrupted spell in 2
#                double.entry=TRUE
#                val2=cur.vals[[spell.row-1]]
#                spell2=matrix(c(terminus, cur.spells[spell.row-1,2]),1,2)
#              }
#              cur.spells[spell.row-1,2]=onset   # truncate interrupted spell
# 	   }
# 	}
# 	# find the minimum spell that is retained (vs. spells that are overlapped/overwritten)
# 	if(terminus>=cur.spells[ns, 2])
# 	  retain.row = ns+1
# 	else
# 	  retain.row = min(which(terminus<cur.spells[,2]))  
# 	# if the terminus interrupts/continues an existing spell...
# 	if(retain.row <= ns && terminus>=cur.spells[retain.row,1]) {
# 	  if(identical(val,cur.vals[[retain.row]])) {
# 	    terminus=cur.spells[retain.row,2]  # forward terminus to that of interrupted spell
# 	    retain.row=retain.row+1
# 	  } else {
# 	    cur.spells[retain.row,1]=terminus  # partial overwrite of interrupted spell
# 	  }
# 	}
# 	# construct new spell matrix and value vector
# 	new.spells = matrix(c(onset, terminus),1,2)
# 	new.values = list(val)
#         if(double.entry){
#           new.spells = rbind(new.spells, spell2)
#           new.values[[2]] = val2
#         }
# 	if(spell.row > 1){
# 	  new.spells = rbind(cur.spells[1:(spell.row-1),], new.spells)
# 	  new.values = c(cur.vals[1:(spell.row-1)], new.values)
# 	}
# 	if(retain.row <= ns) {
# 	  new.spells = rbind(new.spells, cur.spells[retain.row:ns,])
# 	  new.values = c(new.values, cur.vals[retain.row:ns])
# 	}
#       }
#       list(new.values, new.spells)
#    }


