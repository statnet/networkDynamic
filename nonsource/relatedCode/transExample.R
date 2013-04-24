require(networkDynamic)
data(newcomb)  
newcombDyn <- networkDynamic(network.list=newcomb)
# create network, time units are in weeks
# how often do people have a chance to pass ?  
#  have a 1-in-10 chance of passing rumor each week
# stay in descrete time, use week as unit
timeStep <- 1  # since time units are in weeks
transProb <- 0.1 #
forgetProb <- 0.1
# start the rumor out on vertex 1
activate.vertex.attribute(newcombDyn,"heardRumor",TRUE,v=1,onset=0-timeStep,length=timeStep)

# loop through time, updating states
times<-seq(from=0,to=max(get.change.times(newcombDyn)),by=timeStep)
for(t in times){
  # find all the people who knew at last timestep
  knowers <- which(!is.na(get.vertex.attribute.active(newcombDyn,'heardRumor',at=t-timeStep)))
  # get the edge ids of active friendships of people who knew
  for (knower in knowers){
    friendships<-get.edgeIDs.active(newcombDyn,v=knower,at=t)
    for (friendship in friendships){
      # ignore friendships that already passed rumor MEANS NO RE-RUMOR FROM SAME SOURCE
      if (is.null(get.edge.attribute(newcombDyn$mel[friendship],'passedRumor'))){
        # select friendship for transmission with appropriate probability
        if (runif(1)<=transProb){
          # update state of vertices at other end of transmitting edges
          # but we don't know which way the edge points..
          v<-c(newcombDyn$mel[[friendship]]$inl,newcombDyn$mel[[friendship]]$outl)
          # ignore the v we already know 
          v<-v[v!=knower] 
          activate.vertex.attribute(newcombDyn,"heardRumor",TRUE,
                                      v=v,onset=t,length=timeStep)
          # record which frienships the rumor spread across
          set.edge.attribute(newcombDyn,'passedRumor',value=TRUE,e=friendship)
        }
      }
    }
  }  
  # decide who forgets rumor
  knowers <-knowers[runif(length(knowers))>forgetProb]
  # update state of people who didn't forget the rumor
  activate.vertex.attribute(newcombDyn,"heardRumor",TRUE,
                            v=knowers,onset=t,length=timeStep)
}

# how many had heard rumor by week 7
sum(get.vertex.attribute.active(newcombDyn,'heardRumor',at=7),na.rm=TRUE)
# did anybody forget and get reminded?


# problems:  presumeably the underlying friendship network changes more rapidly than once per week, thats just how it was measured.  because we are running the infection sim at faster rate than the edge dynamics.  assumes people don't forget in the same timestep.  Same vertex can be infected twice in one step (not a problem, just slwoer)

# try a version on windsurfers
data(windsurfers)
# start the rumor out on vertex 1
activate.vertex.attribute(windsurfers,"knowsRumor",TRUE,v=1,onset=0-timeStep,terminus=Inf)
activate.vertex.attribute(windsurfers,"heardRumorFrom",1,v=1,onset=0-timeStep,length=timeStep)

runSim<-function(net,timeStep,transProb){
  # loop through time, updating states
  times<-seq(from=0,to=max(get.change.times(net)),by=timeStep)
  for(t in times){
    # find all the people who know and are active
    knowers <- which(!is.na(get.vertex.attribute.active(
      net,'knowsRumor',at=t,require.active=TRUE)))
    # get the edge ids of active friendships of people who knew
    for (knower in knowers){
      conversations<-get.edgeIDs.active(net,v=knower,at=t)
      for (conversation in conversations){
        # select conversation for transmission with appropriate prob
        if (runif(1)<=transProb){
          # update state of people at other end of conversations
          # but we don't know which way the edge points so..
          v<-c(net$mel[[conversation]]$inl,
                 net$mel[[conversation]]$outl)
          # ignore the v we already know 
          v<-v[v!=knower]
          activate.vertex.attribute(net,"knowsRumor",TRUE,
                                    v=v,onset=t,terminus=Inf)
          # record who spread the rumor
          activate.vertex.attribute(net,"heardRumorFrom",knower,
                                  v=v,onset=t,length=timeStep)
          # record which friendships the rumor spread across
          activate.edge.attribute(net,'passedRumor',
                    value=TRUE,e=conversation,onset=t,terminus=Inf)
        }
      }
    }  
  }
  return(net)
}

windsurfers<-runSim(windsurfers,1,0.2)

# to extract infection tree

transTree<-function(net){
  # for each vertex in net who knows
  knowers <- which(!is.na(get.vertex.attribute.active(net,'knowsRumor',at=Inf)))
  # find out who the first transmission was from
  transTimes<-get.vertex.attribute.active(net,"heardRumorFrom",
                                          onset=-Inf,terminus=Inf,return.tea=TRUE)
  # subset to only ones that know
  transTimes<-transTimes[knowers]
  # get the first value of the TEA for each knower
  tellers<-sapply(transTimes,function(tea){tea[[1]][[1]]})
  # create a new net of appropriate size 
  treeIds <-union(knowers,tellers)
  tree<-network.initialize(length(treeIds),loops=TRUE)
  # copy labels from original net
  set.vertex.attribute(tree,'vertex.names',treeIds)
  # translate the knower and teller ids to new network ids   
  # and add edges for each transmission                
  add.edges(tree,tail=match(tellers,treeIds), 
            head=match(knowers,treeIds) )               
  return(tree)                
}
