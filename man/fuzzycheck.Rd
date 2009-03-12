\name{fuzzycheck}
\alias{fuzzycheck}
\encoding{latin1}
\title{ Checking data }
\description{
  It is checked whether the input data file determines a fuzzy number.
}
\usage{
fuzzycheck(H)
}

\arguments{
  \item{H}{Dataset, which must have at least 2 columns.}
}
\details{
For more information about the necessary data format see:  \code{\link{test}}
}
\value{
  The returned value is a number, either 0 or 1. Depending on this an ERROR
  report is delievered.
   \item{0}{The input dataset does not determine a fuzzy number.}
   \item{1}{The input dataset determines a fuzzy number.}
}
\references{R. Viertl, D. Hareter: \emph{Beschreibung und Analyse unscharfer
Information - Statistische Methoden \enc{für}{fur} unscharfe Daten}, Springer, Wien, 2006}
\author{ \enc{Semagül}{Semagul} Aklan, Emine Altindas, Yi Hong Kang, Rabiye Macit, Senay Umar, Hatice \enc{Ünal}{Unal}}

\seealso{ \code{\link{fuzzynumber}}, \code{\link{fuzzymin}}, \code{\link{fuzzymax}}, \code{\link{fuzzydeltacut}}, \code{\link{fuzzysum}}, \code{\link{fuzzymean}}, \code{\link{fuzzyscalar}}, \code{\link{fuzzyproduct}}, \code{\link{fuzzypower}}, \code{\link{fuzzyfunction}}}
\examples{
require(fuzzyOP)
##Example 1:
##create data:
a<-c(-1,0,1,NA,NA,NA)
b<-c(0,1,0,NA,NA,NA)
d<-c(1,2,3,4,5,6)
e<-c(0,0.3,1,0.4,0.2,0)
f<-c(1:5,NA)
g<-c(0,0.2,1,0.5,0,NA)
B<-cbind(a,b,d,e,f,g)
##execute:
fuzzycheck(B)

##Example 2:
data(test)
p<-fuzzycheck(test)
p
}

\keyword{data}
\keyword{datagen}
