\name{fuzzyproduct}
\alias{fuzzyproduct}
\encoding{latin1}
\title{ Product of fuzzy numbers }
\description{
   Product of fuzzy numbers is computed.
}
\usage{
fuzzyproduct(anz, A , vektor, pl)
}

\arguments{
  \item{anz}{ Number of the delta-cuts.  }
  \item{A}{ Dataset, which must have at least 2 columns.}
  \item{vektor}{ Vector, that describes which fuzzy numbers from the dataset are selected. }
  \item{pl}{ Plot-option, 0 or 1 or 2. }
}
\details{
   For more information about the necessary data format see:  \code{\link{test}}
 \cr \cr
  The product of the fuzzy numbers is calculated and if the plot-option is set to 1 the
  characterizing function of the product is depicted. If the option 2 is selected, then
  the characterizing functions of the fuzzy numbers are plotted in one plot and the
  characterizing function of the product in another one.
  If the plot-option is set to 0, then no plot is produced.
}
\value{
  Output data is of the same type as input data.
  Therefore the returned value is a matrix containing the product of fuzzy numbers, which is approximated
  by piecewise linear fuzzy number.  \cr
  The first column describes the x-values and
  the second one the values of the characterizing function of the supporting points
  of the product of the fuzzy numbers.


}
\references{R. Viertl, D. Hareter: \emph{Beschreibung und Analyse unscharfer
Information - Statistische Methoden \enc{für}{fur} unscharfe Daten}, Springer, Wien, 2006  }
\author{ \enc{Semagül}{Semagul} Aklan, Emine Altindas, Yi Hong Kang, Rabiye Macit, Senay Umar, Hatice \enc{Ünal}{Unal}}

\seealso{ \code{\link{fuzzynumber}}, \code{\link{fuzzymin}}, \code{\link{fuzzymax}}, \code{\link{fuzzydeltacut}}, \code{\link{fuzzysum}}, \code{\link{fuzzymean}}, \code{\link{fuzzyscalar}}, \code{\link{fuzzypower}}, \code{\link{fuzzyfunction}}}
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
fuzzyproduct(100,B,c(1,2),1)

##Example 2:
data(test)
A<-fuzzyproduct(5,test,c(1,4,8),2)
A
}

\keyword{math}
\keyword{arith}
\keyword{iplot}
