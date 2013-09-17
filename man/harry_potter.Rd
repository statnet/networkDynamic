%  File networkDynamic/man/windsurfers.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{harry_potter_support}
\alias{harry_potter_support}
\alias{harry_potter}
\docType{data}
\title{
Harry Potter support networks of Goele Bossaert and Nadine Meidert. 
}
\description{
Goele Bossaert and Nadine Meidert have coded the peer-support ties observed between 64 characters in the the text of the well-known J. K. Rowling fictional novels about Harry Potter.
}
\usage{data(harry_potter_support)}
\format{
  The format is is a \link{networkDynamic} object with node and edge \link{activity.attribute}s.
}
\details{
The data in this network was originally collected Goele Bossaert and Nadine Meidert in 2013. They made the data available for general use at \url{http://www.stats.ox.ac.uk/~snijders/siena/HarryPotterData.html} and it was downloaded and converted to a \code{networkDynamic} object.   

The data collection is described by the authors as follows:

Contact between the 64 Hogwarts students was coded as peer support when one of the four types of peer support,
described in Tardy's model, were found: 1) Student A supports student B emotionally, e.g., in Book 1: Harry,
Ron and Hermione assure Neville that he is definitely a Gryffindor when he doubts he is not brave enough to be
part of the house; 2) Student A gives students B instrumental help; e.g., in Book 1: Fred and George Weasley
help Harry Potter to get his trunk into the compartment of the Hogwarts Express; 3) Student A gives student B
certain information to help student B, e.g., in Book 1: Hermione Granger helps Harry Potter with his homework
and; 4) Student A praises student B, e.g., in book 5: Terry Boot praises Hermione Granger, for doing a Protean Charm, which is advanced magic. Furthermore, two extra conditions regarding the context in which peer support appeared needed to be fulfilled as well. 

First, contact between students was only coded if the peer support was offered voluntarily. Second, only
interactions occurring between two living characters, attending Hogwarts at the same moment, were coded as peer support. Consequently, when dead characters reappeared in the books, interactions between these dead characters and living students were not coded. One example for such reappearance is Cedric Diggory's return at the end of book 4, when Cedric asks Harry to return his dead body to his parents. Furthermore, interactions with former or future Hogwarts students at a certain point in time were not included. For example, although Harry and Ginny met before Ginny attended Hogwarts, peer support relations between both characters were only coded when both students attended Hogwarts together.


The network contains the following vertex attributes:

\itemize{
 \item{id:}{ the integer id used by Bossaert and Meidert in their paper} 
 \item{schoolyear:}{ year at which students first attended Hogwarts}
 \item{gender:}{1 is male, 2 is female}
 \item{house:} {number indicating which house student was a member of. 1=Gryffindor, 2=Hufflepuff, 3=Ravenclaw, 4=Slytherin
}
 \item{vertex.names:}{ The full name of each student}
hpnames.txt the names! 

}


}

\source{
Siena datasets: \url{http://www.stats.ox.ac.uk/~snijders/siena/HarryPotterData.html}
}

\references{
Goele Bossaert and Nadine Meidert (2013). 'We are only as strong as we are united, as weak as we are divided'. A dynamic analysis of the peer support networks in the Harry Potter books. \cite{Open Journal of Applied Sciences}, Vol. 3 No. 2, pp. 174-185. \url{http://dx.doi.org/10.4236/ojapps.2013.32024}

}
\examples{
data(harry_potter_support)

# which vertex is Harry Potter?
which(network.vertex.names(harry_potter_support)=="Harry James Potter")

}

\keyword{datasets}
