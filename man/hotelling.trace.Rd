\name{hotelling.trace}
\alias{hotelling.trace}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Compute the hotteling trace statistics.}
\description{
 The trace statistic is computed by matrix multiplication. It depends
 on the contrast matrices, the design matrix and the observations.
}
\usage{
hotelling.trace(X, Y, C, U)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{X}{The design matrix.}
  \item{Y}{The matrix of observations.}
  \item{C}{ The left contrast matrix.}
  \item{U}{The right contrast matrix. }
}
\value{The hotelling trace statistic.
}
\references{
  \item{1}{Steven F. Arnold.The Theory of Linear Models and Multivariate Analysis.
    John Wiley \& Sons.New York, 1981. }
  
  \item{2}{Daniel F. Heitjan, Andrea Manni and Richard J. Santen.Statistical Analysis of in Vivo Tumor Growth Experiments.Cancer Research 1993.53:6042-6050.}
   }
  
  
\author{Raphael Gottardo}

\examples{
data(Bt20)
### Remove the missing values (if any)
Bt20<-remove.NA(Bt20)
###Create the design matrix
X<-design.matrix(Bt20[,1])
###Convert as numeric the measurement
Y<-as.matrix(Bt20[,3:dim(Bt20)[2]])
### Number of groups
p<-dim(X)[2]
### Default contrast matrices
C<-cbind(rep(1,p-1),-diag(p-1))
U<-rbind(rep(1,p),-diag(p))
### Compute the Lawley Hotteling trace statistics
tr<-hotelling.trace(X,Y,C,U)
 }

\keyword{multivariate}
