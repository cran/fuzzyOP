\name{fuzzyfunction}
\alias{fuzzyfunction}
\encoding{latin1}
\title{ Extension principle for fuzzy numbers}
\description{
  A continuous function will be applied to fuzzy numbers.
}
\usage{
fuzzyfunction(f, anz, A, vektor, pl)
}

\arguments{
  \item{f}{A continuous function.}
  \item{anz}{ Number of the delta-cuts.  }
  \item{A}{ Dataset, which must have at least 2 columns.}
  \item{vektor}{ Vector, that describes which fuzzy numbers from the dataset are selected. }
  \item{pl}{  Plot-option, 0 or 1 or 2.  }
}
\details{
  For more information about the necessary data format see:  \code{\link{test}}
 \cr \cr
  If the plot-option is set to 1 the images of the fuzzy numbers under the function \strong{f}
  are calculated (via \strong{extension principle}) and depicted. If the option 2 is selected, then
  the characterizing functions of fuzzy numbers are plotted in one plot and
  the characterizing function of the images in another one.
  If the plot-option is set to 0, then no plot is produced.
}
\value{
  Output data is of the same type as input data.
  The returned value is a matrix containing the images of the function \strong{f} applied
  to the fuzzy numbers.
  The odd columns contain the x-values and the even columns the values of the
  characterizing function of the supporting points of the fuzzy numbers under the function \strong{f}.
}
\references{ R. Viertl, D. Hareter: \emph{Beschreibung und Analyse unscharfer
Information - Statistische Methoden \enc{f�r}{fur} unscharfe Daten}, Springer, Wien, 2006}
\author{ \enc{Semag�l}{Semagul} Aklan, Emine Altindas, Yi Hong Kang, Rabiye Macit, Senay Umar, Hatice \enc{�nal}{Unal}}

\seealso{ \code{\link{fuzzynumber}}, \code{\link{fuzzymin}}, \code{\link{fuzzymax}}, \code{\link{fuzzydeltacut}}, \code{\link{fuzzysum}}, \code{\link{fuzzymean}}, \code{\link{fuzzyscalar}}, \code{\link{fuzzyproduct}}, \code{\link{fuzzypower}}}
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
fuzzyfunction(function(x){x+10},100,B,1,2)

##Example 2:
data(test)
A<-fuzzyfunction(sin,5,test,c(1,4,8),1)
A
}

\keyword{math}
\keyword{optimize}
\keyword{iplot}
