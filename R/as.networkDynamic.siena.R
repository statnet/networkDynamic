
# nodeSets:	List of node sets involved
# observations:	Integer indicating number of waves of data
# depvars:	List of networks and behavior variables
# cCovars: List of constant covariates associated with nodeSets, should be mapped to static vertex attributes
# vCovars: List of changing covariates associated with nodeSets, should be mapped to TEA attributes
# dycCovars: List of constant dyadic covariates, should be mapped to static edge attributes
# dyvCovars: List of changing dyadic covariates, should be mapped to dynamic edge attributes
# compositionChange: List of composition change objects corresponding to the node sets These are lists of intervals and should be mapped to vertex activity


as.networkDynamic.siena<-function(object, verbose=FALSE,...){
  
  if(!'siena'%in%class(object)){
    stop("as.networkDynamic.siena can only be applied to objects of class 'siena' as produced by RSiena::sienaDataCreate")
  }
  sienaData<-object
  waves<-list()  # will be a list of networks for the time points
  behaviorVars<-list() # will be list of behavior variables
 
  for (edgeType in names(sienaData$depvars)){
    # need to change parsing behavior depending on how it is coded in the depvar
    if(attributes(sienaData$depvars[[edgeType]])$type=='oneMode'){
     # this is the edge data  
     # expect that the matrices are listed by (row, col, wave]
     waves<-lapply(1:sienaData$observations,function(t){
       #TODO: should be more efficient do to this via edgelist ..
       arraymat<-sienaData$depvars[[edgeType]][,,t,drop=FALSE]
       # need to convert the array into a matrix
       # TODO: double check if it is by row
       mat<-matrix(sienaData$depvars[[edgeType]][,,t,drop=FALSE],nrow=dim(arraymat)[1],ncol=dim(arraymat)[2],byrow = TRUE) 
       as.network.matrix(mat,matrix.type='adjacency')
     })
    } else if(attributes(sienaData$depvars[[edgeType]])$type=='behavior'){
      # this is dynamic vertex data
      behaviorVars[[edgeType]]<-lapply(1:sienaData$observations,function(t){
        sienaData$depvars[[edgeType]][,1,t]
      })
    } else {
      warning( 'processing depvar type ',attributes(sienaData$depvars[[edgeType]])$type, ' not yet implemented and was skipped')
    }
  }
  
  # activate any of the behavior vars onto the static networks
  # they will be converted to dynamic later
  if(length(behaviorVars)>0){
    for(var in names(behaviorVars)){
      for(t in 1:sienaData$observations){
        set.vertex.attribute(waves[[t]],var,behaviorVars[[var]][[t]])
      }
    }
  }
  
  # convert the list of networks
  dyn<-networkDynamic(network.list=waves,create.TEAs = TRUE)
  
  
  # TODO: it is called nodesets, so probably there can be more than one? need example and produce in loop
  if(length(sienaData$nodeSets)>1){
    stop('multiple nodeSets found in siena data object, not sure how to process')
  } else {
    set.vertex.attribute(dyn,names(sienaData$nodeSets)[1],as.vector(sienaData$nodeSets[[1]]))
    if (verbose) message('mapped nodeSet ',names(sienaData$nodeSets)[1], ' as vertex attribute')
  }
  
  # apply the compositionChange (vertex dynamics)
  # I don't see an example of a compositionChange object with more than one spell per vertex
  # so I'm not clear how these are represented and should be interpreted
  if(length(sienaData$compositionChange)>0){
    # if any of the vectors are longer than 2, I don't know how to interpret them
    if(any(sapply(sienaData$compositionChange$composition,length)!=2)){
      stop('unsure how to process composition change vectors with more than two elements')
    }
    # have to remove existing vertex activity
    delete.vertex.activity(dyn)
    # because the mapping is from a semi-discrete time units with right-closed intervals
    # we will subtract one unit from the onset
    spells<-do.call('rbind',sienaData$compositionChange$composition)
    spells[,1]<-spells[,1]-1
    activate.vertices(dyn,onset=spells[,1],terminus=spells[,2])
    if(verbose) message('mapped compositionChange to vertex activity ')
  }
  
  # copy constant vertex covariates
  if(length(sienaData$cCovars)>0){
    for(covar in names(sienaData$cCovars)){
      #TODO: these need to be matched to associated nodeSet?
      # need to reconstruct these as they have been recentered, add the mean back in
      values<-as.vector(sienaData$cCovars[[covar]])
      valMean<-attr(sienaData$cCovars[[covar]],'mean')
      set.vertex.attribute(dyn,covar,values+valMean)
      if (verbose) message('mapped constant covariate ',covar, ' as vertex attribute')
    }
  }
  
  # copy constant dyadic covariates
  if(length(sienaData$dycCovars)>0){
    for(dyadvar in names(sienaData$dycCovars)){
      #TODO: these need to be matched to associated nodeSet?
      set.edge.value(dyn,dyadvar,as.vector(sienaData$dycCovars[[dyadvar]]))
      if (verbose) message('mapped constant covariate ',dyadvar, ' as edge attribute')
    }
    message('NOTE: dyadic covariates that do not correspond to existing edges are not copied')
  }
  
  return(dyn)
}




