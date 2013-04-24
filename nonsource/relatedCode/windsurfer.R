#Lin Freeman's windsurfer data'

#From Zack and Carter's paper:
#The data analyzed in the following sections was originally collected and
#analyzed in aggregate by Freeman et al. (1988) and has since been used in
#a number of influential articles (see Cornwell, 2009; Hummon and Doreian,
#2003; Zeggelink et al., 1996, etc.). While this network is typically analyzed
#in aggregate, it was originally collected as a dynamically evolving network
#(where the vertex set is composed of windsurfers and the edge set is composed
#of interpersonal communication). The network was collected daily (sampled
#at two time points each day) for 31 days (August 28, 1986 to September 27,
#191986)

#Individuals were tracked with a unique ID, and were divided by Freeman
#et al. into those we will here call “regulars” (N = 54) – frequent attendees
#who were well-integrated into the social life of the beach community – and
#“irregulars” (N = 41) on ethnographic grounds. The former category was
#further broken down by the researchers into two groups, Group 1 (N = 22)
#and Group 2 (N = 21), with 11 individuals not classified as belonging to
#either Group 1 or Group 2. Altogether, the union of vertex sets (Vmax )
#consists of 95 individuals. On any given day during the observation period,
#the number of windsurfers appearing on the beach ranged from 3 to 37, with
#the number of communication ties per day ranging from 0 to 96.
#These basic characteristics will be used in the illustrative analysis that
#follows, which centers on the question of what drives the evolution of inter-
#personal communication in this open, uncontrolled setting.

#from Zack's email:
#Attached is the windsurfer networks (WindSurfers.rda); this is a list
#of network objects (length 31, but there is one missing entry index 25
#marked with an NA). Each list entry is labeled 828, 829 ... 927 --
#this corresponds to the date the network was collected. Vertex
#attributes include: "group1", "group2", "regular", "vertex.names" --
#group1, group2, and regular are ethnographically defined (and are
#simply dummies in this case, i.e., 0/1), and vertex.names is the
#original code number used by Lin. I have also attached several network
#level attributes (e.g., list.network.attributes). This includes
#"atmp", "cord", "day", "gst", "week", "wspd", "wvht"  -- atm, gst,
#wspd, wvht are from the national atmospheric data and come from the
#closest beach I could find that had accurate weather data. day is
#simply the Monday/Tuesday/etc labeling and week is the position within
#the month (e.g., first week in august).

require(network)
require(networkDynamic)
require(sna)
require(ndtv)
require(MASS)


#convert to a network dynamic object
load("windsurferPanels.rda")

days <- c(1:24,26:31)
#find the vertex set with data ids
vert_ids <- numeric()
for (n in days){
  vert_ids <-unique(c(vert_ids,beach[[n]]%v%'vertex.names')) 
}
windsurfers <- network.initialize(length(vert_ids),directed=F)
for (n in days){
  #get the set of active node data ids
  active <- sapply(beach[[n]]%v%'vertex.names', function(x){which(vert_ids==x)})
  windsurfers <- activate.vertices(windsurfers,onset=n-1,terminus=n,v=active)
  #copy attribute data. In this case, the attributes are not dynamic, but not fully specified by any subnetwork
  windsurfers <- set.vertex.attribute(windsurfers,"group1",beach[[n]]%v%"group1",v=active)
  windsurfers <- set.vertex.attribute(windsurfers,"group2",beach[[n]]%v%"group2",v=active)
  windsurfers <- set.vertex.attribute(windsurfers,"regular",beach[[n]]%v%"regular",v=active)
  #copy network vars, don't have a function for this, argg'
  windsurfers <- activate.network.attribute(windsurfers,"atmp",beach[[n]]%n%"atmp",onset=n-1,terminus=n)
  windsurfers <- activate.network.attribute(windsurfers,"day",beach[[n]]%n%"day",onset=n-1,terminus=n)
  windsurfers <- activate.network.attribute(windsurfers,"cord",list(beach[[n]]%n%"cord"),onset=n-1,terminus=n)
  windsurfers <- activate.network.attribute(windsurfers,"gst",beach[[n]]%n%"gst",onset=n-1,terminus=n)
  windsurfers <- activate.network.attribute(windsurfers,"week",beach[[n]]%n%"week",onset=n-1,terminus=n)
  windsurfers <- activate.network.attribute(windsurfers,"wspd",beach[[n]]%n%"wspd",onset=n-1,terminus=n)
  windsurfers <- activate.network.attribute(windsurfers,"wvht",beach[[n]]%n%"wvht",onset=n-1,terminus=n)
  #TODO: handle NAs for missing day
  #copy edges, have to do in loop so we can translate ids
  data_ids <-beach[[n]]%v%'vertex.names'
  for (edge in beach[[n]]$mel){
    from <- which(vert_ids==data_ids[edge$outl])
    to <- which(vert_ids==data_ids[edge$inl])
    #create edge if doesn't exist
    if(windsurfers[from,to] < 1){    
       windsurfers[from,to] <- 1;
       #TODO copy edge attributes.  what does weight mean?
       #windsurfers[from,to]<-get.edge.value(beach[[1]],"weight")[1]
    }
    newEid <- get.edgeIDs(windsurfers,v=from,alter=to)
    windsurfers <- activate.edges(windsurfers,onset=n-1,terminus=n,e=newEid)
  }
       
  
}



