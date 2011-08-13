\name{defaults}
\Rdversion{1.6}
\alias{defaults}
\title{
Default parameters for a given TTR
}
\description{
Almost exclusively used by other functions in the package.
}
\usage{
defaults(ttr)
}
\arguments{
  \item{ttr}{
A character string for built-in TTRs.  Will accept a user-defined
 function for ttr, but in this case 0 is returned
}
}
\details{
The following TTR choices from package 'TTR' are supported:
 - "aroon"
 - "cci"
 - "cmo"
 - "kst"
 - "macd"
 - "tdi"
 - "trix"

Also, the TTR "macd4" is a modificaton of "macd" by the author

TTR "none" returns a position of 0 always, i.e. no position

TTR "hold" returns a position of 1 always, i.e. always long

}
\value{
Outputs several lists of parameters.  Could be of different length 
depending on TTR. Used almost exclusively within other functions.

If the final output value is TRUE, this will force the second parameter
(and 4th, if applicable) to be strictly greater than the first (3rd, resp.)
This is sensible if the pairs are moving average parameters.
}
\references{
http://cran.r-project.org/web/packages/TTR/
}
\author{
David St John
}

\examples{

## "macd4" has an extra parameter which is fixed by default
## to 1 in "macd", but can be varied using "macd4"

defaults("macd")
defaults("macd4")

}
