%  File networkDynamic/man/windsurfers.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2013 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{vanDeBunt_students}
\alias{vanDeBunt_students}
\docType{data}
\title{
van de Bunt longitudinal student friendship dataset
}
\description{
Multiple levels of friendship ties among students reported at 7 time points.  This data set was collected by Gerhard van de Bunt, and is discussed extensively in van de Bunt (1999) and van de Bunt, van Duijn, and Snijders (1999). It is used as example in the manual and in various methodological articles about SIENA. 
}
\usage{data(vanDeBunt_students)}
\format{
  The format is is a \link{networkDynamic} object with node and edge \link{activity.attribute}s.
}
\details{
The dataset was acquired from \url{http://www.stats.ox.ac.uk/~snijders/siena/vdBunt_data.htm}. The information below is adapted from the description page:

 The data were collected among a group of university freshmen who, except for a few existing relationships (acquaintances from a former school), did not know each other at the first measurement (time=t0). The data were collected at 7 time points. The first four time points are three weeks apart, whereas the last three time points are six weeks apart. The original group consisted of 49 students, but due to 'university drop-outs' and after deleting those who did not fill in the questionnaire four or more times, a group was obtained of 32 students for whom almost complete data are available.

The students were asked to rate their relationships on a six point scale, with response categories described as follows.

\describe{
\item{1. Best friendship}{Persons whom you would call your 'real' friends}
\item{2. Friendship}{Persons with whom you have a good relationship, but whom you do not (yet) consider a 'real' friend}
\item{3. Friendly relationship}{Persons with whom you regularly have pleasant contact during classes. The contact could grow into a friendship}
\item{4. Neutral relationship}{Persons with whom you have not much in common. In case of an accidental meeting the contact is good. The chance of it growing into a friendship is not large}
\item{0. Unknown person}{Persons whom you do not know}
\item{5. Troubled relationship}{Persons with whom you can't get on very well, and with whom you definitely do not want to start a relationship. There is a certain risk of getting into a conflict}
  }
  
NOTE: in the import process, ties are not created for value 0. Also, in the original matrix values ``6 = item non-response, 9 = actor non-response'' These  missing data codes were translated to NA values, but not well represented by the conversion to a \code{networkDynamic} as we have not set the spec for dynamic missingness yet.
 
Next to the sociometric data, available individual characteristics are sex, education program, and smoking behavior. Smoking was only allowed in special areas. As a consequence, the 'smokers' had to separate themselves from the 'non-smokers' if they wished to smoke (which they often did during coffee and lunch breaks). Thus, contact opportunities differed between actors because of their smoking behavior. The education program was important because, although all started to study at the same moment, there were three groups, following different courses. During the first months all programs overlapped largely, but after a few months, the programs diverged. Especially the 2-year program was quite different from the other two programs. Therefore, this attribute also gives information on the individuals' contact opportunities.  See the references mentioned belo for further information about this network and the actor attributes. 

The network contains the following vertex attributes:

\itemize{
 \item{gender:}{ integer, 1 is female, 2 is male}
 \item{program:} { integer, 2=2-year, 3=3-year, 4=4-year}
 \item{smoking:}{ integer,1 = yes, 2 = no}
 \item{vertex.names:}{integer matrix id used in the input files}
}

As this network was observed at unequal intervals, the \code{\link{net.obs.period}} attribute provides information on the duration between observation windows. 


}



\source{
Siena datasets: \url{http://www.stats.ox.ac.uk/~snijders/siena/vdBunt_data.htm}
}

\references{
Van de Bunt, G.G. 1999. \cite{Friends by choice. An actor-oriented statistical network model for friendship networks through time.} Amsterdam: Thesis Publishers.

Van de Bunt, G.G., M.A.J. van Duijn, and T.A.B. Snijders. 1999. Friendship networks through time: An actor-oriented statistical network model. \cite{Computational and Mathematical Organization Theory}, 5, 167-192. 

}
\examples{
data(vanDeBunt_students)

}

\keyword{datasets}
