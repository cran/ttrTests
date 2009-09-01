\name{dataSnoop}
\Rdversion{1.1}
\alias{dataSnoop}
\title{
Two Tests for Data Snooping: RC and SPA
}
\description{
Tests for data snooping bias by doing bootstrap resampling, then
finding the best parameterization in the bootstrapped samples and
performing one of two popular tests - White's Reality Check or
Hansen's test for Superior Predictive Ability.  Can write a summary
of results to a file as a LaTeX figure.
}
\usage{
dataSnoop(x, ttr = "macd4", start = 0, nSteps = 0, stepSize = 0, burn = 0, 
short = FALSE, silent = TRUE, TC = 0.001, loud = TRUE, alpha = 0.025, begin = 1, 
percent = 1, file = "", benchmark = "hold", bSamples = 100, test="SPA",latex="")
}
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
  \item{begin}{
The starting index of the data.  The function assumes that the
 user wants a subset of the data, where the default subset
 is the entire data
}
  \item{percent}{
How much of the original data to use (default 100%)
}
  \item{file}{
The full writable path string for a file to which output
 will be appended.  Ideal for reviewing results.
}
  \item{benchmark}{
When computing 'excess' returns, all functions in this package
 subtract the conditional returns based on a given "ttr" from
 the "benchmark" returns.  Two different TTRs can be compared
 this way if desired.
}
  \item{bSamples}{
Number of bootstrapped samples to analyze
}
  \item{test}{
Supports "RC" which is White's Reality Check, or "SPA", which
is Hansen's test for Superior Predictive Ability
}
\item{latex}{
Full path name for a writable file.  The laTeX code that generates a
figure with a summary of the output will be appended to file.
}
}
\value{
Returns the P-value or values from the given test.
}

\references{
Ryan Sullivan, Allan Timmermann, and Halbert White. Data snooping,
technical trading rule performance, and the bootstrap. The Journal of
Finance, 54(5):1647-1691, 1999.

Peter R. Hansen.  A Test for Superior Predictive Ability.  Jounal of
Business and Economic Statistics, 2005.
}
\author{
David St John
}
\note{
See papers for details on the 'V' values an 'T' values

If p-value is significant, then the null hypothesis that good
performance is due to data snooping is rejected.  However, this
does not preclude any other null hypothesis that might explain
good results.

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
rc <- dataSnoop(spData,bSamples=5,test="RC")
spa <- dataSnoop(spData,bSamples=5,test="SPA")

}

\keyword{ design }
