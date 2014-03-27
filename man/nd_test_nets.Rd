\name{nd_test_nets}
\alias{nd_test_nets}
\docType{data}
\title{
networkDynamic testing networks
}
\description{
A list of \code{\link{networkDynamic}} objects to be used for testing package functions. Each network exemplifies a possible extreme case for the data structure representation. 
}
\usage{data(nd_test_nets)}
\format{
  A list of \code{\link{networkDynamic}} objects. Each element is named with a short description
}
\details{
This list of networks is intended for testing purposes.  Each element is a network with a different unusual configuration of the \code{\link{networkDynamic}} data structure.  The idea is that the items on the list can be used when testing functions that use \code{networkDynamic} objects to make sure that they can properly handle (or explicitly reject) a wide range of network configurations and parameter settings. A short description of each test case can be printed with \code{names(nd_test_nets)}. Note that these test cases are intended for checking that functions process the data structures without error, but in most cases the networks are too trivial to be useful in testing algorithm results. 

}

\examples{

data(nd_test_nets)
# print the list of test cases
names(nd_test_nets)
# check that network.size.active works
# (or at least doesn't fail)
# for many types of networks
lapply(nd_test_nets,network.size.active,at=1)

}
\keyword{datasets}
