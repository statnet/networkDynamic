# tests of file importing in various formats
require(networkDynamic)
require(testthat)
pkgpath<-path.package('networkDynamic') # for accessing test files


# ----- read.son import tests ---
# try reading basic file containing alpha ids and with start time but no end time
# also includes a fake cluster, just to mess with things
expect_warning(alphaIdNet<-read.son(paste(pkgpath,'/extdata/alphaIdTest.son',sep='')),regexp="Unable to locate an arc column for 'EndTime'")
expect_equal(as.data.frame(alphaIdNet)$onset,c(0,0))
expect_equal(as.data.frame(alphaIdNet)$terminus,c(0,0))
expect_equal(as.data.frame(alphaIdNet)$tail,c(1,3))
expect_equal(as.data.frame(alphaIdNet)$head,c(2,2))
# did it load pid
expect_equal(alphaIdNet%n%'vertex.pid','vertex.names')
expect_equal(alphaIdNet%v%'vertex.names',c('1','2','three'))


# try reading the extra var test
varsNet<-read.son(paste(pkgpath,'/extdata/extraVarTest.son',sep=''))
# "unchanging" should be static, others active
expect_equal(list.vertex.attributes(varsNet),c("active","Happiness.active","Label.active","na","Unchanging","vertex.names"))
# ArcWeight should be loaded in as a static attriube             
expect_equal(list.edge.attributes(varsNet),c("active","ArcWeight","na"))

# check behavior of guess.TEA
varsNet2<-read.son(paste(pkgpath,'/extdata/extraVarTest.son',sep=''),guess.TEA=FALSE)
# unchanging should be active
expect_equal(list.vertex.attributes(varsNet2),c("active","Happiness.active","Label.active","na","Unchanging.active","vertex.names"))
# ArcWeight should be loaded in as a TEA attriube             
expect_equal(list.edge.attributes(varsNet2),c("active","ArcWeight.active","na"))

# try reading mcfarland classroom with attributes
cls33<-read.son(paste(pkgpath,'/extdata/cls33_10_16_96.son',sep=''))
expect_equal(network.size(cls33),20)
expect_equal(range(get.change.times(cls33)),c(0,49))
expect_equal(list.vertex.attributes(cls33),c("active","BorderColor", "BorderWidth", "ColorName",   "Label", "na", "NodeShape", "NodeSize","vertex.names"))

# check that vertex attributes were parsed correctly
expect_equal(get.vertex.attribute(cls33,'BorderColor'),rep('black',20))
expect_equal(get.vertex.attribute(cls33,'BorderWidth'),rep(1.5,20))
expect_equal(get.vertex.attribute(cls33,'ColorName'),c("gray","gray","gray","darkGray","darkGray","gray","orange" ,  "gray","darkGray","gray","darkGray","gray","darkGray","orange" ,  "gray"  ,   "gray"   ,  "gray"   ,  "gray" ,    "gray"     ,"gray" ))
expect_equal(get.vertex.attribute(cls33,'Label'),c(122658 ,129047, 129340, 119263, 122631, 144843,   1003, 113433, 131642, 139722, 139195, 133105, 116749,      3, 146757, 121402, 127265, 121829, 113140, 128065))
expect_equal(get.vertex.attribute(cls33,'NodeShape'),c("ellipse", "rect",    "rect",    "rect",    "ellipse", "rect",    "rect",    "rect",    "rect",    "ellipse", "ellipse", "rect", "rect",    "rect",    "ellipse", "rect",    "ellipse", "ellipse", "rect",    "ellipse"))
expect_equal(get.vertex.attribute(cls33,'NodeSize'),rep(5,20))

# check that edge attributes parsed correctly
expect_equal(list.edge.attributes(cls33),c("active","ArcWeight.active","ArcWidth.active","ColorName.active","na"))


# test file with no edges

# test file with missing header

# test missing start time

# test missing end time