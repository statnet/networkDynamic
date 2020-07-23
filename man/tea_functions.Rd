%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2013 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{attribute.activity.functions}
\alias{attribute.activity.functions}
\alias{activate.vertex.attribute}
\alias{get.vertex.attribute.active}
\alias{activate.edge.attribute}
\alias{activate.edge.value}
\alias{get.edge.value.active}
\alias{get.edge.attribute.active}
\alias{activate.network.attribute}
\alias{get.network.attribute.active}
\alias{deactivate.edge.attribute}
\alias{deactivate.vertex.attribute}
\alias{deactivate.network.attribute}
\alias{list.vertex.attributes.active}
\alias{list.edge.attributes.active}
\alias{list.network.attributes.active}
\alias{TEAs}
\alias{dynamic.attributes}
\title{
Functions to activate and query dynamic attributes on vertices, edges, and networks
}
\description{
These functions set, query and modify attributes of networkDynamic objects used to represent values which change over time. These dynamic or "temporally extended attributes" are stored as standard attributes with a specially defined structure.
}
\usage{
activate.vertex.attribute(x, prefix, value, onset = NULL, terminus = NULL, 
    length = NULL, at = NULL, v = seq_len(network.size(x)), dynamic.only = FALSE)

activate.edge.attribute(x, prefix, value, onset = NULL, terminus = NULL, 
    length = NULL, at = NULL, e = seq_along(x$mel), dynamic.only = FALSE)
    
activate.edge.value(x, prefix, value, onset = NULL, terminus = NULL, 
    length = NULL, at = NULL, e = seq_along(x$mel), dynamic.only = FALSE)
    
activate.network.attribute(x, prefix, value, onset = NULL, terminus = NULL, 
    length = NULL, at = NULL, dynamic.only = FALSE)    
    
\method{get.vertex.attribute}{active}(x, prefix, onset = NULL, terminus = NULL, 
          length = NULL, at = NULL, rule = c("any", "all","earliest","latest"), 
          na.omit = FALSE, null.na = TRUE, active.default = TRUE,
          dynamic.only = FALSE, require.active = FALSE,
          return.tea = FALSE, unlist = TRUE, ...)   
 
\method{get.edge.attribute}{active}(x, prefix, onset = NULL, terminus = NULL,
          length = NULL, at = NULL, rule = c("any", "all","earliest","latest"), 
          active.default = TRUE, dynamic.only = FALSE, require.active = FALSE, 
          return.tea = FALSE, unlist = TRUE, ..., el) 

\method{get.edge.value}{active}(x, prefix, onset = NULL, terminus = NULL,
          length = NULL, at = NULL, rule = c("any", "all","earliest","latest"), 
          active.default = TRUE, dynamic.only = FALSE, require.active = FALSE, 
          return.tea = FALSE, unlist = TRUE, ...) 

\method{get.network.attribute}{active}(x, prefix, onset = NULL, terminus = NULL,length = NULL, 
                at = NULL, rule = c("any", "all","earliest","latest"), 
                dynamic.only = FALSE, return.tea=FALSE, unlist=TRUE, ...) 
                
deactivate.vertex.attribute (x, prefix, onset=NULL, terminus=NULL,
          length = NULL, at = NULL, v = seq_len(network.size(x)), dynamic.only = FALSE)
                 
deactivate.edge.attribute (x, prefix, onset = NULL, terminus = NULL,
          length = NULL, at = NULL, e = seq_along(x$mel), dynamic.only = FALSE)

deactivate.network.attribute(x, prefix, onset = NULL, terminus = NULL, 
          length = NULL, at = NULL, dynamic.only = FALSE)
                
\method{list.vertex.attributes}{active}(x, onset = NULL, terminus = NULL,length = NULL, 
                at = NULL, na.omit = FALSE, rule = c("any", "all"), 
                v = seq_len(network.size(x)), require.active = FALSE, 
                active.default = TRUE, dynamic.only = FALSE, ...)
                
\method{list.edge.attributes}{active}(x, onset = NULL, terminus = NULL,length = NULL, 
                at = NULL, na.omit = FALSE, rule = c("any", "all"), 
                e = seq_along(x$mel), require.active = FALSE, 
                active.default = TRUE, dynamic.only = FALSE, ...)
                
\method{list.network.attributes}{active}(x, onset = NULL, terminus = NULL,length = NULL, 
                at = NULL, na.omit = FALSE, rule = c("any", "all"),
                dynamic.only = FALSE, ...)

}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 a \code{networkDynamic} object
}
  \item{prefix}{
 character string for the name of the attribute to be stored (will be stored as prefix.active)
}

  \item{value}{
 an object that is the value of the attribute to be stored. If \code{value} is a \code{list}, it is assumed that each element corresponds to an index in \code{v}.
}
  \item{onset}{
an optional vector of time-points that specifies the starts of the interval(s) at which the \code{value} argument should be considered active for each vertex/edge.
}
  \item{terminus}{
 an optional vector of time-points that specifies the ends of the interval(s) at which the \code{value} argument should be considered active for each vertex/edge.
 }
 \item{length}{
 an optional vector of interval lengths for the interval(s) which should be activated for each vertex/edge. This must be accompanied by one of \code{onset} or \code{terminus}.
 }
 \item{at}{
 optional, one or more time points (corresponding to vertices/edges) at which values should be activated for each vertex/edge
 }
  \item{v}{
optional, one or more IDs indicating which vertices should have the TEA attribute added (default is all vertices)
}

 \item{e}{
 optional, one or more edge IDs indicating which edge should have the TEA attribute added. Default is all edges. Note: Currently values of \code{e} correspond to elements of \code{x$mel}, including null elements from deleted edges. So if some edges have been deleted from the network, \code{e} may need to be padded out appropriately and can be longer than \code{network.edgecount(x)}. 
 }
 \item{dynamic.only}{
  If dynamic.only==TRUE, only TEAs are considered; otherwise, non-TEA attributes are also employed (with activity as defined by active.default). 
 }
 
 \item{require.active}{
   logical. If TRUE, NA will be returned instead of an attribute value if the associated vertex or edge is inactive for the query period. When vertex or edge activity has not be explicitly set, the state is controlled by \code{active.default}.
 }
 
 \item{active.default}{
   logical; should underlying vertices or edges without an activity attribute be regarded as active by default?
 }

 \item{na.omit}{
  logical; should values from missing vertices/edges be removed? (implemented for vertices, but removal means that the index may not correspond to vertex.id for returned values)
 }
 
 \item{null.na}{
   logical; should \code{NULL} values be replaced with NAs? 
 }
 
 \item{rule}{
   a character string indicating how the attribute activity spells should be matched against the query spell for this query: 
   \itemize{
     \item \code{any} (default) match attribute values if they overlap with any part of the query interval.  
     \item \code{all} match an attribute value only if it overlaps with the entire query interval, 
     \item \code{earliest} match a single value corresponding to the earliest attribute overlaping any part of the query spell, 
     \item \code{latest} match a single value corresponding to the earliest attribute overlaping any part of the query spell.
   }
   
 }
 
 \item{return.tea}{
   logical; if \code{return.tea=TRUE}, will return an (appropriately truncated) activity attribute- a list with the first element a list of values and the second element the spell matrix of corresponding spell(s) matching according to \code{rule}. Also sets \code{unlist=FALSE} and \code{dynamic.only=TRUE}. 
 }
 
 \item{unlist}{
   logical; should retrieved attributes be unlisted prior to being returned? Setting to FALSE is necessary to correctly fetch list objects stored as attributes.
 }
 
 \item{\dots}{
   possible additional arguments
 }
 
 \item{el}{
   unused argument that exists solely for consistency with \code{network}; do not use this argument!
 }
}
\details{
These functions provide easy ways to work with temporally extended attributes, making it possible to store attributes on a dynamic network that should be represented as changing values and different points in time. Because the serve as wrappers for get.*.attribute and set.*.attribute, many of the arguments are directly passed through. 

The activate.*.attribute methods act as a cross between  activate.*  set.*.attribute methods.  They are used to activate an attribute for a given spell, and in so doing set a value for that spell.  The corresponding deactivate methods are more straightforward, deactivating the attribute over a set interval (and removing any spells/values as needed).

A dynamic or Temporally Extended Attribute (TEA) is an edge, vertex, or network attribute satisfying the following properties:

\enumerate{
 \item Its name consists of an arbitrary prefix, together with the suffix ".active".
 \item Its value consists of a two-element list, whose respective contents must be maintained in order as follows:
 \itemize{
	    \item a. A list of value entries, such that the ith list element is the value of the attribute for the ith spell in the associated activity matrix.  An attribute not active at any given point in time is defined as having a value of NA (but the reverse is not true -- an active attribute can also take a value of NA).
	    \item b. A two-column numeric matrix, conforming to the specifications for activity attributes in the networkDynamic package.  Every active spell indicated in the activity matrix must correspond to exactly one entry of the associated value list, and these must be maintained in order (i.e., the value of the attribute for the ith active spell is the ith element in the value list).
      }
}

There are several ways to specify the activity spell, and the general syntax rules are described at \code{\link{activity.attribute}}.  Activity can be set for a single time point, using either \code{at}, or by setting onset=terminus.  Activity can be set for an interval [onset,terminus), using a valid combination of the \code{onset}, \code{terminus} and \code{length} attributes. 

  
This allows for a wide range of options when specifying spells, but a correct specification must use only one of these forms:
\preformatted{
  at
  onset and terminus 
  onset and length
  terminus and length
}

If the \code{set} and \code{deactivate} functions are provided with no timing information, the element is activated/deactivated from \code{-Inf} to \code{Inf}. The specified interval spans the period from the onset (inclusive) to the terminus (exclusive), so \code{[onset,terminus)}. For more details, see \code{\link{activate}} The \code{get} query functions will give an error if no timing information is provided. 


When the activity arguments have a length greater than 1, the elements are assumed to correspond to the vertex or edge ids that should have that activity specified, not to the elements of \code{value}. In other words, it is possible to use one function call to activate multiple values on multiple vertices with a different activity time on each vertex, but it is not possible to activate multiple values at multiple times on a single vertex with one call. 


\code{activate.edge.value} is a wrapper for \code{activate.edge.attribute}. 


When the attribute query has a non-zero duration (i.e. \code{length} > 0, or \code{onset!=terminus}) it is possible that the query spell will match against multiple attribute values for each network element. If multiple values match and \code{return.tea=FALSE}, a warning will be generated and only the earliest value will be returned. To avoid the warning, this behavior (or its opposite) can be specified by setting \code{rule='earliest'} or \code{rule='latest'}.
     
}
\value{
The set methods modify their network argument internally and also invisibly return a reference to the modified network. The get methods return the attribute object appropriate for the time range specified query spell parameters.  If query results in multiple values and \code{return.tea=FALSE} only the first (earliest) is returned along with a warning. Note that \code{get.edge.value.active} returns \code{NULL} if no attributes names match, where \code{get.vertex.attribute.active} returns \code{NA}s.  Also of \code{null.na=FALSE} and \code{unlist=TRUE}, the index position for returned values may not correspond to the vertex.id because NULL values will be omited.
}
%\references{
%See statnet internal TEA spec documents \url{https://statnet.csde.washington.edu/trac/wiki/TemporallyExtendedAttributes}
%}
\author{
skyebend, ayn, carter, pavel, martina, steve
}
\note{
Attribute activity is only checked against vertex and edge activity during fetching if \code{require.active=TRUE}.  Otherwise the attributes for a vertex can take values (be active) when the vertex itself is inactive. Likewise for edges.  The because they must check all attributes on all network elements, the \code{list.*.active} functions are generally more computationally expensive that a \code{get} call for checking if a specific attribute exists. Note that  \code{get.edge.attribute.active} currently calls \code{get.edge.attribute.active} and does not support a list of edges as input. 
}


\seealso{
See Also as \code{\link{set.vertex.attribute}}, \code{\link{get.vertex.attribute}},\code{\link{list.vertex.attributes}},\code{\link{activate.vertices}}, \code{\link{activity.attribute}}, \code{\link{activate}}
}
\examples{
#initialize network
test<-network.initialize(5)

#activate vertex attribute
test<-activate.vertex.attribute(test,"letter","a",onset=0,terminus=1)
test<-activate.vertex.attribute(test,"number","4",onset=1,terminus=2)
test<-activate.vertex.attribute(test,"number","5",onset=2,terminus=3)

#list active/all vertex attributes
list.vertex.attributes.active(test, onset=0,terminus=3,dynamic.only=TRUE)
list.vertex.attributes.active(test, onset=1,terminus=3,dynamic.only=FALSE)

#get values for specific vertex attribute
get.vertex.attribute.active(test,"letter",onset=2,terminus=3)

#deactive vertex attribute
test <- deactivate.vertex.attribute(test, "letter", onset=0, terminus=3)
list.vertex.attributes.active(test, onset=0,terminus=3,dynamic.only=TRUE)

#initialize edges
test[1,2]<-1
test[2,3]<-1

#activate edge attribute
test<-activate.edge.attribute(test,"number",1,onset=0,terminus=2)
test<-activate.edge.attribute(test,"number",5,onset=2,terminus=5)
test<-activate.edge.attribute(test,"letter","a",onset=1,terminus=4)

#list edge attributes
list.edge.attributes.active(test, onset=0,terminus=4,dynamic.only=TRUE)
list.edge.attributes.active(test, onset=0,terminus=4,dynamic.only=FALSE)

#get values for specific edge attribute
get.edge.value.active(test,"number",onset=3,terminus=4)

#deactive edge attribute
test <- deactivate.edge.attribute(test, "letter", onset=0, terminus=3)
list.edge.attributes.active(test, onset=0,terminus=3,dynamic.only=TRUE)

#activate network attribute
test <- activate.network.attribute(test,"alist",list("a","b"),onset=1,terminus=2)
test <- activate.network.attribute(test,"alist",list("c","d"),onset=2,terminus=3)
test <- activate.network.attribute(test,"aspace",list("1","2"),onset=1,terminus=2)
test <- activate.network.attribute(test,"aspace",list("3","4"),onset=2,terminus=3)

#list network attributes
list.network.attributes.active(test, onset=0,terminus=3,dynamic.only=TRUE)
list.network.attributes.active(test, onset=0,terminus=3,dynamic.only=FALSE)

#get values for specific network attribute
get.network.attribute.active(test,"alist",at=2.5,unlist=FALSE)

#deactive network attribute
test <- deactivate.network.attribute(test, "alist", onset=0, terminus=3)
list.network.attributes.active(test, onset=0,terminus=3,dynamic.only=TRUE)

# activate multiple values on multiple vertices at multiple times
test<-network.initialize(3)
activate.vertex.attribute(test,"letters",c("a","b","c"),onset=c(0,1,2),terminus=c(1,2,3))

# peek at TEA structure using non-TEA attribute query
get.vertex.attribute(test,"letters.active",unlist=FALSE)

# compare different 'rules'
test<-network.initialize(1)
activate.vertex.attribute(test,'color','red',onset=0,terminus=1)
activate.vertex.attribute(test,'color','green',onset=1,terminus=2)
activate.vertex.attribute(test,'color','blue',onset=2,terminus=3)

get.vertex.attribute.active(test,'color',onset=0,terminus=3)
get.vertex.attribute.active(test,'color',onset=0,terminus=3,return.tea=TRUE)
get.vertex.attribute.active(test,'color',onset=0,terminus=3,rule='earliest')
get.vertex.attribute.active(test,'color',onset=0,terminus=3,rule='latest')

}

