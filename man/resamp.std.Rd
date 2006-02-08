\name{resamp.std}
\alias{resamp.std}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Compute the standard deviation by bootstrap.}
\description{
 Bootstrap a new data set from the original data set by keeping the
 general structure of the groups. Then compute the standard deviation
 based on the bootstrap estimates.
}
\usage{
resamp.std(factors, Y, X, B=1000)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{factors}{A vector (character or numeric) containing the group ids.}
  \item{Y}{The matrix of measurements.}
  \item{X}{The design matrix.}
  \item{B}{A numeric value, the number of bootstraps.}
}
\value{The matrix of standard deviation.
}
\author{Raphael Gottardo}

 

\examples{
data(Bt20)
###Create the design matrix
X<-design.matrix(Bt20[,1])
###Convert as numeric the measurement
Y<-as.matrix(Bt20[,3:dim(Bt20)[2]])
### Number of groups
p<-dim(X)[2]
### Compute the standard deviation associated with the mean
std<-resamp.std(Bt20[,1],Y,X,B=1000)

}
\keyword{multivariate}
