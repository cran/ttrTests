\name{paramPersist}
\Rdversion{1.1}
\alias{paramPersist}
\title{
Robustness, or Persistence, of Parameter Choices for TTR
}
\description{
Divides the given data set into subperiods and computes the relative
 performance of several different parameterizations of the TTR
 in each subperiod.  These are checked for correlation using
 standard linear regression.
}
\usage{
paramPersist(x, ttr = "macd4", start = 0, nSteps = 0, stepSize = 0, 
burn = 0, short = FALSE, silent = TRUE, TC = 0.001, loud = TRUE, 
alpha = 0.25, periods = 2, file = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A univariate series
}
  \item{ttr}{
The TTR to be used.  Can be a character string for built-in TTRs,
 or a user defined function whose output is a position series s(t).
 See 'defaults' for a list of built-in TTRs.
}
  \item{start}{
Initial values for parameters
}
  \item{nSteps}{
How many parameter choices to use for each parameter
}
  \item{stepSize}{
The difference between successive choices of a parameter.
}
  \item{burn}{
When computing the position function s(t), values for t < burn
 will be forced to 0, i.e. no position held during the 'burn' period
}
  \item{short}{
Logical.  If false the position function s(t) will be forced to
 0 when it would otherwise be -1, i.e. no short selling
}
  \item{silent}{
Logical.  If TRUE, supresses output from subroutines
}
  \item{TC}{
Percentage used to compute returns adjusted for trading costs.
}
  \item{loud}{
Logical.  If FALSE, supresses output from the main function(s)
}
  \item{alpha}{
Confidence level for 2-sided hypothesis testing
}
  \item{periods}{
How many periods to split up the original data.
}
  \item{file}{
The full writable path string for a file to which output
 will be appended.  Ideal for reviewing results.
}
}
\details{
See the help file for 'paramStats' for important information about
using "start,nSteps,stepSize" to define a domain of parameters
}
\value{
The first entries of the output list are the CR outputs for
the corresponding sub-periods of the given data.

The next entries (up to 5) are the correlation coefficients
corresponding to the given lag.

The next entries (up to 5) are the p-values for these
correlation coefficients.

So, for example, if periods = 4, the output will be a list
of length 10.  The first 4 items in the output list will
be conditional returns for the 4 subperiods.  The next
3 will be the correlation coefficients out to lag 3.  The
last 3 will be the p-values for these coefficients.
}
\references{
This test was invented by the author to the best of his knowledge.
}
\author{
David St John
}
\note{
EXTREMELY IMPORTANT NOTE: The functions in this package evaluate past 
performance only.  No warranty is made that the results of these tests should, 
or even can, be used to inform business decisions or make predictions of 
future events.  

The author does not make any claim that any results will predict future 
performance.  No such prediction is made, directly or implied, by the outputs of 
these function, and any attempt to use these function for such prediction is done 
solely at the risk of the end user. 
}
\examples{

spData <- as.vector(getYahooData("SPY",start="20060101",end="20081231")[,"Close"])
pp <- paramPersist(spData,periods=8)

length(pp)
pp[[9]]

## should return 7 different correlation coefficients, each lag 1

pp[[14]]

## should return the corresponding 7 p-values
}

\keyword{ design }