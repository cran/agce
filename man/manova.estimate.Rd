\name{manova.estimate}
\alias{manova.estimate}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Compute the mean estimates.}
\description{
 A simple matrix multiplication to compute the mean estimate.
}
\usage{
manova.estimate(Y, X)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{Y}{The matrix of obervations.}
  \item{X}{The design matrix.}
}
\value{The matrix of estimates.
}
\references{Steven F. Arnold.The Theory of Linear Models and Multivariate Analysis.
  John Wiley \& Sons.New York, 1981.}
\author{Raphael Gottardo}

\examples{
data(Bt20)
### Remove the missing values (if any)
Bt20<-remove.NA(Bt20)
###Create the design matrix
X<-design.matrix(Bt20[,1])
###Convert as numeric the measurement
Y<-as.matrix(Bt20[,3:dim(Bt20)[2]])
manova.estimate(Y, X)
}
\keyword{multivariate}
