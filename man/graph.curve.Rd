\name{graph.curve}
\alias{graph.curve}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Graph the medians tumor volumes at each time point and for each group.}
\description{
  Estimate the growth curve for each group. All curves are graphed on
  the same figure.
}
\usage{
graph.curve(data, time, legend=TRUE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{data}{ The data matrix or data frame, where the first column
    correspond to the group ids, the second to the animal ids, the
    remaining columns are the measurements at each time.}
  \item{time}{A vector of numeric values corresponding to the time
    points at which the measurements were effectued.}
  \item{legend}{A logical  value, if TRUE legends using the group
    ids, are added to the graph.}
}
\value{
  The graph added to the current plotting device.
}
\author{Raphael Gottardo}

\examples{
data(Bt20)
graph.curve(Bt20, c(0,3,7,10,14,16), legend=TRUE)
}
\keyword{multivariate}
