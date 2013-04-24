\name{is.active}
\alias{is.active}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Determine Activation State of Edges and Vertices }
\description{
  Query the activity state of one or more edges and vertices for either a single time point or an extended interval.
}
\usage{
is.active(x, onset, terminus = NULL, e = NULL, v = NULL, 
    rule = c("any", "all"), active.default = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{\link[network]{network}}. }
  \item{onset}{ either a single time point, or the onset of the sampling interval. }
  \item{terminus}{ optionally, the end of the sampling interval. }
  \item{e}{ a vector of edge IDs to evaluate. }
  \item{v}{ a vector of vertex IDs to evaluate. }
  \item{rule}{\code{any} if elements active at any time during the interval are to qualify, or \code{all} if elements must be active over the entire interval.  }
  \item{active.default}{logical; should elements without an activity attribute be regarded as active by default?  }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  A logical vector indicating the activity states.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{activity.attribute}}, \code{\link{activate}} }
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
