\name{remove.NA}
\alias{remove.NA}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Remove the rows with missing values.}
\description{
  If one row of the data matrix contains missing measurements, the row
  is delete.
}
\usage{
remove.NA(data)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{data}{The matrix of measurements}
}
\value{
  The data matrix (missing values removed).
}
\author{Raphael Gottardo}

\examples{
data(Bt20)
remove.NA(Bt20)
}
\keyword{NA}
