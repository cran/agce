\name{manova.growth}
\alias{manova.growth}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Performs a multivariate analysis of variance for growth curve experiments.}
\description{
  Performs a multivariate analysis of variance for growth curves
  experiments. By default we test the hypothesis of no group*time interactions.
}
\usage{
manova.growth(data, C=NULL, U=NULL, normal=TRUE, B=1000)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{data}{The data matrix or data frame, where the first column
    correspond to the group ids, the second to the animal ids, the
    remaining columns are the measurements at each time. }
  \item{C}{The left contrast matrix.  Default to test no time*group interactions. }
  \item{U}{The right contrast matrix.  Default
    to test no time*group interactions.}
  \item{normal}{A logical value, if \code{normal} if true the standard
    deviations of the rates are computed based on normal assumption of
    the measurements, otherwise computed by bootstraping. Default to TRUE.}
  \item{B}{A numeric value, the number of bootstraps. Only used if
    \code{normal} is FALSE.}
 
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

\seealso{\code{\link{growth.curve}},  \code{\link{ano.difference}}}

\examples{
data(Bt20)
manova.growth(Bt20, C=NULL, U=NULL, normal=TRUE)
}

\keyword{multivariate}
