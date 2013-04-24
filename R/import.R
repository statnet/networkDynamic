#scripts for loading in dynamic network file formats



#load a .son format file
#many features not supported yest
read.son <- function(file){
  
  #find the index of the header line
  nodeStartLine <- grep("NodeId",readLines(file))
  if (length(nodeStartLine) < 1){
    #try the other alternate
    nodeStartLine <- grep("AlphaId",readLines(file))
    stop("Reading AlphaIds in .son file not yet supported")
  }
  #find the index of the edges split
  arcStartLine <- grep("FromId",readLines(file))
  noderows <- read.table(file,header=TRUE,sep="\t",as.is=TRUE,skip=nodeStartLine-1,nrows=arcStartLine-(nodeStartLine+1))
  arcrows <- read.table(file,header=TRUE,sep="\t",as.is=TRUE,skip=arcStartLine-1)
  #figure out the order of the node column headings
  idIndex <-1
  nodeStartIndex <- which(names(noderows)=="StartTime")
  nodeEndIndex <- which(names(noderows)=="EndTime")
  #figure out the order of arc column headings
  fromIdIndex <-1
  toIdIndex <- 2
  arcStartIndex <- which(names(arcrows)=="StartTime")
  arcEndIndex <- which(names(arcrows)=="EndTime")
  return(ndConverter(list(noderows[,c(idIndex,nodeStartIndex,nodeEndIndex)],arcrows[,c(fromIdIndex,toIdIndex,arcStartIndex,arcEndIndex)])))
}
