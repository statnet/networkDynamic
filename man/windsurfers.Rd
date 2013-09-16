%  File networkDynamic/man/windsurfers.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{windsurfers}
\alias{windsurfers}
\alias{beach}
\docType{data}
\title{
Lin Freeman's Dynamic Network of Windsurfer Social Interactions
}
\description{
A dynamic network object describing daily social interactions between windsurfers on California beaches over a months time. Collected by Lin Freeman in 1988.
}
\usage{data(windsurfers)}
\format{
  The format is is a \link{networkDynamic} object with node and edge \link{activity.attribute}s defined by spell matricies.
}
\details{
The data in this network was originally collected and
analyzed in aggregate by Freeman et al. (1988) and has since been used in
a number of influential articles (see Cornwell, 2009; Hummon and Doreian,
2003; Zeggelink et al., 1996, etc.). While this network is typically analyzed
in aggregate, it was originally collected as a dynamically evolving network
(where the vertex set is composed of windsurfers and the edge set is composed of interpersonal communication). 

The network was collected daily (sampled at two time points each day) for 31 days (August 28, 1986 to September 27,1986). From Almquist and Butts, 201:

"Individuals were tracked with a unique ID, and were divided by Freeman
et al. into those we will here call, 'regulars' (N = 54) -- frequent attendees
who were well-integrated into the social life of the beach community -- and 'irregulars' (N = 41) on ethnographic grounds. The former category was further broken down by the researchers into two groups, Group 1 (N = 22) and Group 2 (N = 21), with 11 individuals not classified as belonging to
either Group 1 or Group 2. Altogether, the union of vertex sets (Vmax )
consists of 95 individuals. On any given day during the observation period,
the number of windsurfers appearing on the beach ranged from 3 to 37, with
the number of communication ties per day ranging from 0 to 96." 

The dynamicNetwork object was created from  a list of network objects 
(length 31, but there is one missing entry index 25 marked with an NA). Each list entry is labeled 828, 829 ... 927 -- this corresponds to the date the network was collected. Vertex
attributes include: group1, group2, regular, vertex.names --
group1, group2, and regular are ethnographically defined (and are
simply dummies in this case, i.e., 0/1), and vertex.names is the original code number used by Lin. This are static vertex attributes. 

The original set of static networks have been merged into a single dynamic network object with vertex activity coded in a spell matrix following the conventions of networkDynamic objects. There is very high daily turnover of who is present on the beach on each day. 

Several dynamic network level attributes (e.g., list.network.attributes) have been added by Zack Almquist. These include:
 
\itemize{
\item \code{atmp} atmospheric temperature. 
\item \code{cord} Grid location on the beach.
\item \code{day} simply the Monday/Tuesday/etc labeling.
\item \code{gst} ground surface temperature (celsius).
\item \code{week} week is the position within the month (e.g., first week in august)
\item \code{wspd} Wind speed from noaa.gov.
\item \code{wvht} Wave height noaa.gov.
}

atm, gst, wspd, wvht are from the national atmospheric data (noaa.gov) and come from the
closest beach that had accurate weather data. Day of week is relevant because weekly periodicity of the nodeset is quite high.
}

\source{
Dr. Lin Freeman (Research Professor UCI) and Zack W Almquist (Graduate Student, UCI)
}

\references{
Almquist, Zack W. and Butts, Carter T. (2011). "Logistic Network Regression for Scalable Analysis of Networks with Joint Edge/Vertex Dynamics." IMBS Technical Report MBS 11-03, University of California, Irvine. \cr

Freeman, L. C., Freeman, S. C., Michaelson, A. G., 1988. "On human social
intelligence." Journal of Social Biological Structure 11, 415--425.

}
\examples{
data(windsurfers)

data(windsurferPanels)
}

\keyword{datasets}
