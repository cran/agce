\name{growth.curve}
\alias{growth.curve}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Perform an anova on slopes.}
\description{
  This function is particular a case of the manova.growth, the manova
  reduces to an ANOVA when the a particular growth curve is linear.
}
\usage{
growth.curve(data, C=NULL, U=NULL, normal=T, B=1000, time=NULL)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{data}{ The data matrix or data frame, where the first column
    correspond to the group ids, the second to the animal ids, the
    remaining columns are the measurements at each time. }
  \item{C}{ The left contrast matrix. Default to test same rate of growth in each group.}
  \item{U}{ The right contrast matrix. Default to test same rate of growth in each group.}
  \item{normal}{A logical value, if \code{normal} is true the standard
    deviations of the rates are computed based on normal assumption of
    the measurements, otherwise computed by bootstraping. Default to TRUE.}
  \item{B}{A numeric value, the number of bootstraps. Only used if
    \code{normal} is FALSE. }
  \item{time}{A vector of numeric values corresponding to the time
    points at which the measurements were effectued. \code{Time} is absolutely required.}
}
\details{ In this function, we assume that the mean is of the form
  A+B*t. Where A, B are estimated . Therefore the mean is just a function
  (matrix multiplication ) of the rates.
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
\references{Daniel F. Heitjan, Andrea Manni and Richard J. Santen
  Statistical Analysis of in Vivo Tumor Growth Experiments
  Cancer Research 53:6042-6050, 1993.}
\author{Raphael Gottardo}

\seealso{ \code{\link{manova.growth}}}

\examples{
data(Bt20)
growth.curve(Bt20, C=NULL, U=NULL, normal=T, time=c(0,3,7,10,14,16))
}
\keyword{multivariate}
