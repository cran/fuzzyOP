\name{fuzzyscalar}
\alias{fuzzyscalar}
 \encoding{latin1}
\title{ Multiplication of fuzzy numbers with a scalar}
\description{
  Fuzzy numbers are multiplied with a scalar.
}
\usage{
fuzzyscalar(anz, A, vektor, v, pl)
}

\arguments{
  \item{anz}{ Number of the delta-cuts. }
  \item{A}{ Dataset, which must have at least 2 columns. }
  \item{vektor}{ Vector, that describes which fuzzy numbers from the dataset are selected.  }
  \item{v}{ A scalar. }
  \item{pl}{ Plot-option, 0 or 1 or 2. }
}
\details{
  For more information about the necessary data format see:  \code{\link{test}}
 \cr \cr
  The multiplication of the fuzzy numbers with a scalar is calculated and if the
  plot-option is set to 1 the characterizing function of the multiplication with a scalar is depicted.
  If the option 2 is selected, then
  the characterizing functions of the fuzzy numbers are plotted in one plot and the
  characterizing function of the multiplication with a scalar in another one.
  If the plot-option is set to 0, then no plot is produced.
}
\value{
  Output data is of the same type as input data.
  Therefore the returned value is a matrix containing the multiplication of fuzzy numbers with a scalar, which
  are again piecewise linear fuzzy numbers. \cr
  The odd columns contain the x-values and the even columns the values of the
  characterizing function of the supporting points
  of the multiplication of the fuzzy numbers with a scalar.
}
\references{R. Viertl, D. Hareter: \emph{Beschreibung und Analyse unscharfer
Information - Statistische Methoden \enc{für}{fur} unscharfe Daten}, Springer, Wien, 2006 }
\author{ \enc{Semagül}{Semagul} Aklan, Emine Altindas, Yi Hong Kang, Rabiye Macit, Senay Umar, Hatice \enc{Ünal}{Unal}}

\seealso{ \code{\link{fuzzynumber}}, \code{\link{fuzzymin}}, \code{\link{fuzzymax}}, \code{\link{fuzzydeltacut}}, \code{\link{fuzzysum}}, \code{\link{fuzzymean}}, \code{\link{fuzzyproduct}}, \code{\link{fuzzypower}}, \code{\link{fuzzyfunction}} }
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
fuzzyscalar(100,B,1,5,2)

##Example 2:
data(test)
A<-fuzzyscalar(5,test,c(1,4,8),4,1)
A
}

\keyword{math}
\keyword{arith}
%\keyword(iplot)
