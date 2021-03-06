\name{fuzzypower}
\alias{fuzzypower}
\encoding{latin1}
\title{ Power of fuzzy numbers}
\description{
  The power of the fuzzy numbers is computed.
}
\usage{
fuzzypower(anz, A , vektor, pot, pl)
}

\arguments{
  \item{anz}{ Number of the delta-cuts. }
  \item{A}{ Dataset, which must have at least 2 columns.}
  \item{vektor}{ Vector, that describes which fuzzy numbers from the dataset are selected. }
  \item{pot}{ A Power. }
  \item{pl}{ Plot-option, 0 or 1 or 2. }
}
\details{
  For more information about the necessary data format see:  \code{\link{test}}
 \cr \cr
  The power of the fuzzy numbers is calculated and if the plot-option is set to 1
  the power is depicted. If the option 2 is selected, then
  the characterizing functions of the fuzzy numbers are plotted in one plot and the
  characterizing functions of the power in another one.
  If the plot-option is set to 0, then no plot is produced.
}
\value{
  Output data is of the same type as input data.
  Therefore the returned value is a matrix containing the power of fuzzy numbers, which are approximated
  by piecewise linear fuzzy numbers.
  The odd columns describe the x-values and
  the even ones the values of the characterizing function of the supporting points
  of the power of the fuzzy numbers .

}
\references{ R. Viertl, D. Hareter: \emph{Beschreibung und Analyse unscharfer
Information - Statistische Methoden \enc{f�r}{fur} unscharfe Daten}, Springer, Wien, 2006 }
\author{ \enc{Semag�l}{Semagul} Aklan, Emine Altindas, Yi Hong Kang, Rabiye Macit, Senay Umar, Hatice \enc{�nal}{Unal}}

\seealso{ \code{\link{fuzzynumber}}, \code{\link{fuzzymin}}, \code{\link{fuzzymax}}, \code{\link{fuzzydeltacut}}, \code{\link{fuzzysum}}, \code{\link{fuzzymean}}, \code{\link{fuzzyscalar}}, \code{\link{fuzzyproduct}}, \code{\link{fuzzyfunction}}}
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
fuzzypower(100,B,1,3,1)

##Example 2:
data(test)
A<-fuzzypower(5,test,c(1,4,8),3,2)
A
}

\keyword{math}
\keyword{arith}
\keyword{iplot}
