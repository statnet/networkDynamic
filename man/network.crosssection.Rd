\name{network.crosssection}
\alias{network.crosssection}
\alias{\%t\%}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Temporal Cross-sections of Dynamically Extended Network Objects }
\description{
  Given a specified time point (or interval), return a reduced network containing only active vertices and edges.
}
\usage{
network.crosssection(x, onset, terminus = NULL, 
    rule = c("any", "all"), active.default = TRUE)
x \%t\% onset
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{\link[network]{network}}. }
  \item{onset}{ either a single time point, or the onset of the sampling interval. }
  \item{terminus}{ optionally, the end of the sampling interval. }
  \item{rule}{ \code{any} if elements active at any time during the interval are to be used, or \code{all} if elements must be active over the entire interval. }
  \item{active.default}{ logical; should elements without an activity attribute be regarded as active by default? }
}
\details{
  If only an onset value is specified, the returned network consists of all edges and vertices active at that time.  If a terminus is also supplied, edges and vertices active over the specified interval are used.  If \code{rule=="any"}, then edges and vertices active at any time during the interval are employed; otherwise, only those active during the entire period are used.
  
  \code{\%t\%} (the temporal cross-section operator) is simply a simplified (and less flexible) wrapper for \code{network.crosssection}.
}
\value{
  A \code{network} object containing the active edges and vertices for the specified time period.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Carter T. Butts \email{buttsc@uci.edu}}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{is.active}}, \code{\link{activity.attribute}}, \code{\link{network.extensions}} }
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
