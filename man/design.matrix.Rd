\name{design.matrix}
\alias{design.matrix}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Create the design matrix associated to the data. }
\description{}
\usage{
design.matrix(factors)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{factors}{The vector (first column of the data) of group ids.}
}
\value{The design matrix X.
}
\author{Raphael Gottardo}

\examples{
data(Bt20)
### Remove the missing values (if any)
Bt20<-remove.NA(Bt20)
X<-design.matrix(Bt20[,1])
}
\keyword{htest}