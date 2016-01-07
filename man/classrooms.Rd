%  File networkDynamic/man/classrooms.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{cls33_10_16_96}
\alias{cls33_10_16_96}
\alias{McFarland_cls33_10_16_96}
\docType{data}
\title{
Daniel McFarland's Streaming Classroom Interactions Dataset
}
\description{
A dynamic network object describing continuous time streams of social interactions between teachers and students in a classroom observed by Daniel McFarland in 1996.
}
\usage{data(McFarland_cls33_10_16_96)}
\format{
  The format is is a \link{networkDynamic} object  named \code{cls33_10_16_96} with node and edge \link{activity.attribute}s defined by spell matricies and associated vertex attributes and dynamic edge attributes. 
}
\details{
The dynamic network object has 20 vertices and directed edges and covers a time range of 0.0 to 49.0 minutes of class time. The vertex attribute \code{gender} gives gender ('M' or 'F') of each person and \code{type} gives a crude role ('grade11','grade12','instructor').  The original dataset id for the vertices is included as \code{data_id} and defined as a persistent.id. The edges include a dynamic attribute (TEA) \code{interaction_type} as a factor with levels 'saction', 'social' and 'task' (see explanation below) and a \code{weight} assigned by the reacher to that interaction (direct comunication is given more weight than indirect 'broadcast' communication).

A .son formatted input file with the same data as well as seperate tsv files for vertices and edges are located in the /inst/extdata directory.

The data for these classroom interactions consists of streaming observations of conversation turns. The conversation turns were recorded as pairs of senders and receivers and for types of content. Speakers were viewed as directing their communication in one of two fashions: (1) indirect soundings, such as lectures (where a teacher addresses all students); and (2) direct interactions that are focused on particular others. Each type of directional speech is viewed as having different forms of network reception - indirect speech reaches all bystanders as passive hearers and directed speech reaches the focal person as an active co-author of the conversation (Goffman 1981).

Two types of interaction are found to prevail in classroom contexts: task and sociable (McFarland, in press). Task interactions are those behaviors that pertain to the ongoing teacher prescribed task (content is academic). In contrast, sociable interactions concern everyday concerns of adolescents' social lives, such as parties, dating, social outings, plans, etc. While the content is the key distinction, it is often the case that these speech acts are distinguishable in style as well, where sociable behaviors are more play-like, fast-paced, and free than the more constrained academic styles of speech during lessons (Cazden 1988). Last, observations also recorded when task and sociable forms of interaction were laminated with evaluative meaning. Such evaluations were seen as being either positive or negative - either giving praise or attempting a reprimand (Ridgeway and Johnson).

The class (#33) is an economics class composed of 11th and 12th graders at a magnet high school. On this day, economics has two teachers. The first is the usual teacher and the second is a businessman who donates his time a couple days a month to assist the class with their lesson on stock investments. After a minute of undefined class time, the two teachers prescribe collaborative group work and assist students in conducting it. The students are assigned groups within which they are to study the stock market and make mock investments they can follow over time. The groups compete with each other over the course of the semester to see who can make the greatest profit.

The network narrative for the class opens with the teacher trying to focus student attention and then lecturing while the visiting professional assists each group. The teacher prescribes group work but continues to lecture, thereby preventing the desired social routine from fully taking effect. Eventually the students are doing group projects while the adults move from group to group, facilitating their progress, and stabilizing interaction patterns. The routine of group work is basically characterized by dyadic task and social interactions that persist in multiple clusters. Not all persons engage in these groups, and a couple students sit quietly by themselves. The group work routine breaks down as social activity increases within the groups', and then the teacher emits broadcast sanctions in an effort to redirect student attention back on task (16 minutes). The task breaks down again at the end of class, but this time because the adults make closing announcements.

So the network illustrates that teachers are involved in this task engaging their students as they monitor interaction. When students become too social, a teacher usually arrives, disperses the group, and then reforms it via task interactions (revolution in type of behavior, McFarland 2004). Hence, the ``dance'' here entails relatively bounded groups of individuals that free-associate over tasks and drift into social affairs, and teachers who refocus affairs by indirect means of broadcasts or by direct means of directed speech. 
}

\source{
Dr. Daniel McFarland, Stanford University \url{http://ed.stanford.edu/faculty/mcfarland}
}

\references{
    McFarland, Daniel A. 2005. ``Why Work When You Can Play? Dynamics of Formal and Informal Organization in Classrooms.'' Chapter 8 in The Social Organization of Schooling, edited by Larry Hedges and Barbara Schneider (pp. 147-174). New York: Russell Sage Foundation.
    
    McFarland, Daniel A. 2004. ``Resistance as a Social Drama - A Study of Change-Oriented Encounters.'' American Journal of Sociology 109 (6): 1249-1318.
    
    McFarland, Daniel A. 2001. ``Student Resistance: How the Formal and Informal Organization of Classrooms Facilitate Everyday Forms of Student Defiance.'' American Journal of Sociology 107 (3): 612-78.
    
    McFarland, Daniel A. and Skye Bender-deMoll. 2003. ``Classroom Structuration: A Study of Network Stabilization.'' Working paper, Stanford University.
}
\examples{
data(McFarland_cls33_10_16_96)
}

\keyword{datasets}
