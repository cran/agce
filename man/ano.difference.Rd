\name{ano.difference}
\alias{ano.difference}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Performs a simple anova on a vector of measurement (or differences).}
\description{
This function is a particular case of the manova.growth, the manova
reduces to an ANOVA when the matrix of measurement is a vector. If an
initial time is specified (see initial), the anova is performed on the
differences and rates of growth might be computed.
}
\usage{
ano.difference(data, initial=NULL, final="last", C=NULL, U=NULL, normal=TRUE, B=100, rate=TRUE,time=NULL)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{data}{The data matrix or data frame, where the first column
    correspond to the group ids, the second to the animal ids, the
    remaining columns are the measurements at each time. }
  \item{initial}{An integer corresponding to the index of the initial time. If
    \code{initial} is specified, the anova is performed on the
    differences. Otherwise only the 'final' measurements are used.}
  \item{final}{ An integer corresponding to the index of the final time. By
    default \code{final} is equal to "last" which correspond to the last
  measurement.}
  \item{C}{The left contrast matrix. Default to the matrix to test equal
  means in each group.}
  \item{U}{The right contrast matrix. Default to the identity. }
  \item{normal}{ A logical value, if \code{normal} is true the standard
    deviations of the rates are computed based on normal assumption of
    the measurements, otherwise computed by bootstraping. Default to TRUE.}
  \item{B}{A numeric value, the number of bootstraps. Only used if
    \code{normal} is FALSE. }
  \item{rate}{A logical  value, if TRUE,  the rates are computed. Default
    to TRUE. The rate can not be computed if \code{initial} is not
    specified. }
  \item{time}{A vector of numeric values corresponding to the time
    points at which the measurements were effectued. Time is absolutely required.}
  
  
}

\value{
  \item{Ftest}{The value of the F-test used to  test the hypothesis
    based on C and U.}
  \item{df1}{The numerator degrees of freedom for the F-test}
  \item{df2}{The denominator degrees of freedom for the F-test}
  \item{p}{The p-value associated to the F-test}
  \item{mean}{The estimate of the mean}
  \item{std.mean}{The standard deviation associated to the mean}
  \item{rate}{The rate of growth. Set to NULL if \code{rate} is FALSE}
  \item{std.rate}{The standard deviation of the rates. Set to NULL if \code{rate} is FALSE}
}
\author{Raphael Gottardo}


\seealso{ \code{\link{manova.growth}},\code{\link{growth.curve}}}

\examples{
data(Bt20)
ano.difference(Bt20, initial=1, final="last", C=NULL, U=NULL, normal=TRUE, rate=TRUE,time=c(0,3,7,10,14,16))
}

\keyword{univar}
