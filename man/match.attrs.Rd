\name{match.vertex.attrs}
\alias{match.vertex.attrs}
\alias{match.edge.attrs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 return the times at which the TEA attributes of elements of a network match a value
}
\description{
The functions query the TEA attributes of network elements (vertices or edges) and return the time that each element matches the specified value.  The match operator defaults to '==', but other binary comparison operators that return logical, such as '>' or even '\%in\%' can be used as well. 
}
\usage{
match.vertex.attrs(nd, attrname, value, match.op = "==", rule = "earliest", 
                        no.match = Inf, v = seq_len(network.size(nd)))
                        
match.edge.attrs(nd, attrname, value, match.op = "==", rule = "earliest", 
                        no.match = Inf, e = seq_along(nd$mel))                        
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
 a \code{networkDynamic} object
}
  \item{attrname}{
character name of the TEA attribute to be queried
}
  \item{value}{
the value to be passed to the comparison operator to be matched against the TEA values
}
  \item{match.op}{
the binary operator to be used in determining a match (defaults to '==')
}
  \item{rule}{
character giving the name of the rule to determine what value should be returned from the spell of matching attributes. Options are \code{'earliest'} (the default) and \code{'latest'} which will return the onset and terminus of the spell respectively. 
}
  \item{no.match}{
The value to be returned when the TEA attribute never matches \code{value}.  Default is \code{Inf}, \code{NA} may be useful as well. 
}
  \item{v}{
  possible numeric vector of vertex ids indicating a subset of vertices to be queried
  }
  \item{e}{
  possible numeric vector of edge ids indicating a subset of edges to be queried
  }
}
\details{
The \code{no.match} argument is included to make it possible for user to distinguish cases where the attribute is missing from the network element (which will return \code{NA}) from cases where the value of the attribute never matches \code{value} according to the operation \code{match.op}
}
\value{
A numeric vector of attribute onset times (if \code{rule='earliest'}) or termination times (if \code{rule='latest'})
}

\author{
skyebend
}
\note{
this is a draft implementation, suggestions on function and argument names welcome. 
}


\seealso{
See also \link{attribute.activity.functions}
}
\examples{

# create a network with TEA attribute
net<-network.initialize(5)
net<-activate.vertex.attribute(net,'test','A',onset=0,terminus=2)
net<-activate.vertex.attribute(net,'test','B',onset=2,terminus=3)
net<-activate.vertex.attribute(net,'test','C',onset=4,terminus=5)

net<-activate.vertex.attribute(net,'weight',0,onset=-Inf,terminus=Inf)
net<-activate.vertex.attribute(net,'weight',-1,at=6,v=2)

# when does 'test' take the value 'B' for each vertex?
match.vertex.attrs(net,'test',value='B')

# when is the terminus of when 'test' takes a value 'A' or 'C'?
match.vertex.attrs(net,'test',value=c('A','C'),match.op='\%in\%',rule='latest')

# when does 'weight' first take a value less than 0?
match.vertex.attrs(net,'weight',value=0,match.op='<')

}

