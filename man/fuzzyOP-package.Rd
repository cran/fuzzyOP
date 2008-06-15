\name{fuzzyOP-package}
\alias{fuzzyOP-package}
\alias{fuzzyOP}
\docType{package}
\encoding{latin1}
\title{
Fuzzy numbers and main operations
}
\description{
This package contains functions which allow to display fuzzy numbers.
Besides, the main arithmetic operations are applied to fuzzy numbers.
The \strong{extension principle} for continuous functions can be applied and plotted.  \cr
The functions in this package need a special dataset as input, which contains a discretization
of the piecewise linear fuzzy numbers. Each fuzzy number is represented by two columns.
The first column of each fuzzy number describes the x-values of the supporting points and the second column consists
of the corresponding values of the characterizing function (Char.f).
If the number of supporting points of the fuzzy numbers in the dataset varies, NAs should be added to the bottom of
the columns.
The functions in this package omit NAs automatically. For more information about NA see \code{help(NA)}.
}

\details{
\tabular{ll}{
Package: \tab fuzzyOP\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2008-05-19\cr
License: \tab FREE\cr
}
}

\author{Maintainer: Aklan \enc{Semagül}{Semagul}, Altindas Emine, Kang Yi Hong, Macit Rabiye, Umar Senay, \enc{Ünal}{Unal} Hatice,
\cr \email{fuzzyOP@gmx.at}
}
\references{
 R. Viertl, D. Hareter: \emph{Beschreibung und Analyse unscharfer
 Information - Statistische Methoden \enc{für}{fur} unscharfe Daten}, Springer, Wien, 2006}
}

\keyword{ package }

