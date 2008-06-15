\name{fuzzydeltacut}
\alias{fuzzydeltacut}
\encoding{latin1}
\title{ Delta-cuts of fuzzy numbers }
\description{
  Delta-cuts of fuzzy numbers are computed.
}
\usage{
fuzzydeltacut(del, A, vektor, pl)
}

\arguments{
  \item{del}{ Vector of delta values}
  \item{A}{Dataset, which must have at least 2 columns.}
  \item{vektor}{Vector, that describes which fuzzy numbers from the dataset are selected. }
  \item{pl}{Plot-option, either 0 or 1. }
}
\details{
For more information about the necessary data format see:  \code{\link{test}}
 \cr \cr
 \strong{del} is a vector of delta values (i.e. between 0 and 1), of which the delta-cuts should be computed.
 \cr
 Only if the \strong{plot-option} is set to 1, the characterizing functions of the fuzzy
 numbers are plotted.
 If the plot-option is set to 0, then no plot is produced.
}
\value{
 The returned value is a matrix: The first column contains the delta values,
 the second one the lower limits of the delta-cuts and the third column
 the upper limits.
}
\references{ R. Viertl, D. Hareter: \emph{Beschreibung und Analyse unscharfer
Information - Statistische Methoden \enc{für}{fur} unscharfe Daten}, Springer, Wien, 2006 }
\author{ Aklan \enc{Semagül}{Semagul}, Altindas Emine, Kang Yi Hong, Macit Rabiye, Umar Senay, \enc{Ünal}{Unal} Hatice}

\seealso{  \code{\link{fuzzynumber}}, \code{\link{fuzzymin}}, \code{\link{fuzzymax}}, \code{\link{fuzzysum}}, \code{\link{fuzzymean}}, \code{\link{fuzzyscalar}}, \code{\link{fuzzyproduct}}, \code{\link{fuzzypower}}, \code{\link{fuzzyfunction}} }
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
fuzzydeltacut(c(0,0.5,1),B,1,1)

##Example 2:
data(test)
A<-fuzzydeltacut(0.5,test,c(1,4,8),1)
A
}


\keyword{iplot}

