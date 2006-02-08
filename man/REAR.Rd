\name{REAR}
\alias{REAR}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Fit a autoregressive model with random errors}
\description{This function fits two different models using the nlme
  library and then compares them using a likelihood ratio test. The first
  model is an autoregressive random errors model with a linear mean
  assuming different slope in each group. The second
  model is an autoregressive random errors model with a linear mean
  assuming same slope in each group. 
  
}
\usage{
REAR(data,time)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{data}{The data matrix or data frame, where the first column
    correspond to the group id, the second to the animal id, the
    remaining columns are the measurement at each time. }
  \item{time}{A vector of numeric values corresponding to the time
    points at which the measurements were effectued.}
}
\details{
 The data are reshaped in a longitudinal dataset and then the models are
 fit using the lme function of the nlme library.
}
\value{
   \item{Chi}{The value of the Chisquare test used to  test the hypothesis
    of equal slopes}
  \item{df}{The degrees of freedom for the Chisquare test}
  \item{p}{The p-value associated to the Chisquare test}
  \item{sum.fit.full}{The summary on the fit of the full model}
  \item{std.fit.reduced}{The summary on the fit of the reduced model}
 
}
\references{Daniel F. Heitjan, Andrea Manni and Richard J. Santen
  Statistical Analysis of in Vivo Tumor Growth Experiments
  Cancer Research 53:6042-6050, 1993.}
\author{Raphael Gottardo}
\examples{
data(Bt20)
REAR(Bt20,c(0,3,7,10,14,16))
}
\keyword{ts}
\keyword{multivariate}
