\name{fuzzysum}
\alias{fuzzysum}
\encoding{latin1}
\title{ Sum of fuzzy numbers }
\description{
  The sum of fuzzy numbers is computed and, if required, the fuzzy numbers are plotted.
}
\usage{
fuzzysum(anz, A , vektor, pl)
}

\arguments{
  \item{anz}{ Number of the delta-cuts.}
  \item{A}{ Dataset, which must have at least 2 columns. }
  \item{vektor}{ Vector, that describes which fuzzy numbers from the dataset are selected.}
  \item{pl}{Plot-option, 0 or 1 or 2}
}
\details{
  For more information about the necessary data format see:  \code{\link{test}}
 \cr \cr
  The sum of the fuzzy numbers is computed, and if the plot-option is set to 1,
  the characterizing function of the sum is depicted. If the option 2 is selected, then
  the characterizing functions of the fuzzy numbers are plotted in one plot and the
  characterizing function of the sum in another one.
  If the plot-option is set to 0, then no plot is produced.
}
\value{
  Output data is of the same type as input data.
  Therefore the returned value is a matrix containing the sum of fuzzy numbers, which is again
  a piecewise linear fuzzy number.  \cr
  The first column describes the x-values and
  the second one the values of the characterizing function
  of the supporting points of the sum of the fuzzy numbers.

}
\references{ R. Viertl, D. Hareter: \emph{Beschreibung und Analyse unscharfer
Information - Statistische Methoden \enc{für}{fur} unscharfe Daten}, Springer, Wien, 2006 }
\author{Aklan \enc{Semagül}{Semagul}, Altindas Emine, Kang Yi Hong, Macit Rabiye, Umar Senay, \enc{Ünal}{Unal} Hatice}

\seealso{\code{\link{fuzzynumber}}, \code{\link{fuzzymin}}, \code{\link{fuzzymax}}, \code{\link{fuzzydeltacut}}, \code{\link{fuzzymean}}, \code{\link{fuzzyscalar}}, \code{\link{fuzzyproduct}}, \code{\link{fuzzypower}}, \code{\link{fuzzyfunction}}}
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
fuzzysum(100,B,c(1,2),2)

##Example 2:
data(test)
A<-fuzzysum(5,test,c(1,4,8),1)
A
}

\keyword{math}
\keyword{arith}
\keyword{iplot}
