\name{test}
\alias{test}
\docType{data}
\title{Example of a dataset}
\description{
  This dataset is a matrix (nrow=10, ncol=16), which contains 8 fuzzy numbers. Each
  piecewise linear fuzzy number is described in 2 columns.
  The first column of each fuzzy number describes the x-values of the supporting points and the second column
  the corresponding values of the characterizing function (Char.f).
  The columns of the x-values of the supporting points must be in ascending order.
  The columns of the values of the characterizing function have to contain values
  between 0 and 1 and must contain the value 1.
  Moreover the first element and the last numerical element of these columns have
  to be 0 (note that the last element could be NA).
  These columns
  have also to be in ascending order until 1 is achieved and
  after that the values must be decreasing.
  If the number of supporting points of the fuzzy numbers in the dataset varies, NAs should be added to the bottom of
  the columns. For more information about NA see \code{help(NA)}.
}
\usage{data(test)}
\examples{
require(fuzzyOP)
data(test)
A<-fuzzynumber(5,test,c(1:5),1)
A
}
\keyword{datasets}
