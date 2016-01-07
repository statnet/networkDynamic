# R script to load and format the mcfarland clasroom networks
library(networkDynamic)
vertexData <-read.table(system.file('extdata/cls33_10_16_96_vertices.tsv', package='networkDynamic'),header=T)
edgeData <-read.table(system.file('extdata/cls33_10_16_96_edges.tsv', package='networkDynamic'),header=T)
clss33new<-networkDynamic(vertex.spells=vertexData[,c(3,4,1)],edge.spells = edgeData[,c(3,4,1,2,5,6)],create.TEAs = TRUE,edge.TEA.names = c('weight','interaction_type'))

# set net obs period
nobs <-clss33new%n%'net.obs.period'
nobs$time.unit<-'minutes'
clss33new%n%'net.obs.period'<-nobs


# load up vertex data 
set.vertex.attribute(clss33new,"data_id",vertexData$data_id)
set.vertex.attribute(clss33new,"gender",as.character(vertexData$sex))
set.vertex.attribute(clss33new,"type",as.character(vertexData$role))

# define persistant id
clss33new%n%'vertex.pid'<-'data_id'


# check that the spell matricies and attributes matches the old version
data('McFarland_cls33_10_16_96')
all.equal(as.data.frame(clss33new),as.data.frame(cls33_10_16_96))

cls33_10_16_96<-clss33new
save(cls33_10_16_96,file='data/McFarland_cls33_10_16_96.rda')
