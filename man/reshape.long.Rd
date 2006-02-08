\name{reshape.long}
\alias{reshape.long}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Reshape the data set in a longitudinal data set in order to use
  the lme function.}
\description{}

\usage{
reshape.long(data, time)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{data}{ The data matrix or data frame, where the first column
    correspond to the group ids, the second to the animal ids, the
    remaining columns are the measurements at each time. }
  \item{time}{A vector of numeric values corresponding to the time
    points at which the measurements were effectued.}
}
\value{
  The reshaped data.
}

\author{Raphael Gottardo}

\examples{
data(Bt20)
long.data<-reshape.long(Bt20,c(0,3,7,10,14,16))
}
\keyword{multivariate}
