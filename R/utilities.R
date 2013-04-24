################################################
################################################
######## utilities.R
######## Author: Zack W. Almquist
######## Date: 02/21/2012
########
######## Includes:
########
########           ndConverter
########           ndConverter.network.series
########           ndConverter.data.frame
########           ndConverter.list
########		     c.network #overloaded c operator for network objects
########
########			misc helper functions not for general use
########
################################################
################################################



###########################################################
### ndConverter functions
###########################################################

##############
### ndConverter
### generic function for managing conversion; i.e., a generic wrapper for onverter.*
##############
ndConverter<-function(x,...){
	UseMethod("ndConverter")
}

##############
### ndConverter
### convers networkDynamic objects to other formats
##############

ndConverter.network<-function(x,format="edgelist",...){do.call(paste(format,"networkDynamic",sep="_"),list(x))}


################
### start networkDynamic-> other formats
################
edgelist_networkDynamic<-function(x){
	tm<-lapply(x$mel,function(y){
	active<-y$atl$active
	ac<-matrix(rep(cbind(y$outl,y$inl),nrow(active)),nc=2,byrow=TRUE)
	cbind(ac,active)
	})
	out<-vector()
	for(i in 1:length(tm)){out<-rbind(out,tm[[i]])}
	out<-cbind(out,out[,4]!=Inf,out[,4]-out[,3])
	colnames(out)<-c("Ego","Alter","Start","End","Noncensored","duration")
	out<-data.frame(out)
	out
}
################
### end networkDynamic-> other formats
################



##############
### lndConverter.network.list
### converts a network.series object to networkDynamic
##############
ndConverter.network.list<-function(x,...){
	x<-duration.matrix(x)
	out<-ndConverter(x)
	out<-addAttributes(out,x$networks) ## add atributes
	out
}


##############
### ndConverter.data.frame
### converts a edgelist objects to networkDynamic
### assumes format from duration.matrix function in ergm
##############

#### Need to add Noncensored check
#### set to inf+te
ndConverter.data.frame<-function(x,...){
	net<-network(unique(x[,1:2]),matrix.type="edgelist")
	men<-mapen(net,x[,1:2])
	x<-cbind(men,x)
	networkDynamicInternal(net,x)
}

##############
### ndConverter.list
### converts a list of network objects to networkDynamic and back
##############
ndConverter.list<-function(x,...){
	warning("Keeps only attributes on first network! TEA methods coming soon.\n")
	if(is.network(x[[1]])){
		out<-listSpell(x,buildNet(x))
		## Check if vertex length is same if so copy attributes
		if(length(unique(sapply(x,function(y){if(!is.network(x)){return(NULL)}
			network.size(y)})))==1){out<-addAttributes(out,x[[1]])}
		return(out)
		}
	
	net<-networkDynamicInterV(x[[1]])
	edges<-unique(x[[2]][,1:2])
	net<-add.edges(net,edges[,1],edges[,2])
	temp<-cbind(mapen(net,x[[2]][,1:2]),x[[2]]) 
	out<-networkDynamicInternal(net,temp)
	
	if(is.network(x[[1]])){
	if(length(unique(sapply(x,network.size)))==1){
		out<-addAttributes(out,x[[1]])
	}
	}
	## Check for vertex dynamics
	## if no vertex dynamics add attributes
	## addAttributes(out,)
	###
	out
}



###########################################################
### Helper functions, internal functions
### These do not need to exported if a specific NAMESPACE file is
### provided
###########################################################

#### copies attributes over
addAttributes<-function(x,net){
	x%n%"directed"<-net%n%"directed"
	m<-match(x%v%"vertex.names",net%v%"vertex.names")
	attr<-list.vertex.attributes(net)
	for(i in 1:length(attr)){
		out<-set.vertex.attribute(x,attr[i],(net%v%attr[i])[m])
		}
	out
}

### Spell dynamics from vertex list
networkDynamicInterV<-function(x){
	warning("All non-unique spells to Vertices dropped")
	nam<-unique(x$NodeId)
	net<-network.initialize(length(nam))
	net%v%"vertex.names"<-nam
	net<-activate.vertices(net,onset=x[,2],terminus=x[,3],v=x[,1])	
	### need to manage issue of multiple spells in one vertex
	net
}



##### add spell to bn using x
listSpell<-function(x,bn){
	test1<-sapply(x,is.network)
	test2<-sapply(x,function(x){ifelse(is.network(x),x%n%"mnext">1,FALSE)})
	for(i in 1:length(x)){
		if(test1[i]){if(test2[i]){bn<-addSpellnl(x[[i]],bn,i-1,i)}}
		}
	bn
}

### adds spell 
addSpellnl<-function(net,bn,time1,time2){
	### Edge Spell
	elt<-mvnum(snel(as.matrix(net,"edgelist")),bn%v%"vertex.names") ## builds edgelist 
	eids<-mapen(bn,elt)
	os<-rep(time1,length(eids))
	ts<-rep(time2,length(eids))
	bn<-activate.edges(bn,onset=os,terminus=ts,e=eids)
	
	### Vertex spell
	vid<-match(net%v%"vertex.names",bn%v%"vertex.names")
	os<-rep(time1,length(vid))
	ts<-rep(time2,length(vid))
	bn<-activate.vertices(bn,onset=os,terminus=ts,v=vid)
	bn
}

### builds basis network
buildNet<-function(x){
	nam<-getNames(x)
	base<-baseNetwork(nam,x[[1]])
	nel<-namedEL(x)
	m<-mvnum(nel,nam)
	base<-add.edges(base,tail=m[,1],head=m[,2])
	base
}

### gets unique namelist
getNames<-function(ln){
out<-unique(unlist(lapply(ln,function(x){if(is.network(x)){return(x%v%"vertex.names")}})))
out[order(out)]
}

#### copies network attributes to empty network
baseNetwork<-function(nam,x){
out<-network.initialize(length(nam), directed = x%n%"directed", hyper = x%n%"hyper", loops = x%n%"loops", multiple = x%n%"multiple", bipartite = x%n%"bipartite")
out%v%"vertex.names"<-nam
out
}

### single named edge list
snel<-function(el){cbind(attr(el,"vnames")[el[,1]],attr(el,"vnames")[el[,2]])}


### full named edgelist
namedEL<-function(ln){
	fout<-vector()
for(i in 1:length(ln)){
	if(is.network(ln[[i]])){
		out<-as.matrix(ln[[i]],"edgelist")
		fout<-rbind(fout,snel(out))
		}
	}
unique(fout)
}

## match named edgelist to id'd edgelist
mvnum<-function(nel,nam){
cbind(match(nel[,1],nam),match(nel[,2],nam))
}

#### Builds edge index map 
mapen<-function(net,e){apply(e,1,function(y){get.edgeIDs(net,v=y[1],alter=y[2])})}

###############
#### Builds a networkDynamic object from edgelist + spell and network object
###############
networkDynamicInternal<-function(net,x){

dupf<-function(x,y,v=">"){as.numeric(names(y)[do.call(v,list(y,x))])}

if("Noncensored"%in%colnames(x)){x[x$Noncensored!=1,5]<-Inf}

net<-activate.edges(net,onset=x[,4],terminus=x[,5],e=x[,1])

###########
## Step 2: add multispells
###########
tab<-table(x[,1])
### 2.1 add all 2< spells
if(any(tab>1)){
for(i in 2:max(tab)){
	index<-dupf(i,tab,"==")
	if(sum(index)!=0){
	temp<-x[x[,1]%in%index,]
	temp<-temp[order(temp[,1]),]
	#cat("i",i,"\n")
	for(j in 1:i){
	t<-temp[seq(j,nrow(temp),i),]
	net<-activate.edges(net,onset=t[,4],terminus=t[,5],e=t[,1])	
	}
	#cat(" j",j,"\n")
	}
}
}
net
}














